# synthid

**Cross-year person linkage and synthetic ID assignment for nonprofit compensation panels.**

`synthid` takes a stacked, multi-year panel of IRS Form 990
director/officer/trustee compensation records — the parsed output of the
[`titleclassifier`](https://github.com/) and `peopleparser` packages — and
assigns every record a **stable per-person identifier (`EMP_ID`)** that is
consistent across tax years. This is the keystone a longitudinal panel design
rests on: the raw `person.id` in the source data is *not* stable across years
(in the 2019–2021 development panel, all 5,133 name+EIN keys that recur across
years carry conflicting `person.id`s), so a dedicated linkage layer is required.

## Why a separate package

The upstream packages answer "what is this record?" (a parsed name, a
standardized title). `synthid` answers "**who** is this record, across years?"
It depends only on the columns those packages emit and is deliberately kept
independent so the linkage logic can be tested and versioned on its own.

## The pipeline

```
link_panel()
  ├─ prepare_panel()      normalize names, build a unique per-record key
  ├─ link_year_pairs()    block on org → compare → score → one-to-one match
  └─ resolve_clusters()   close links into person clusters, stamp EMP_ID
```

1. **Blocking.** Candidate pairs are formed only within an organization (`ein`)
   and only across *different* tax years. Blocks are tiny (median 9 records per
   org-year), so every cross-year pair within an org is compared — no
   first-letter blocking, which would only cost recall here.

2. **Comparison.** Categorical fields (gender, suffix, salutation) use exact
   match; middle name uses Jaro–Winkler. The surname and first name use dedicated
   comparators:
   - **Surname** (`compare_two_names()`): max of a whole-string match (robust to
     separator noise: `MCLANE` ↔ `MC-LANE`) and a coverage token-match divided by
     the *smaller* token count (robust to a dropped/added/reordered token:
     `ANDREWS-MCLANE` ↔ `MCLANE`). Dividing by the smaller count is deliberate:
     two different multi-token surnames sharing one token (`WEINER-COHEN` ↔
     `COHEN-GANTSOUDES`) cannot masquerade as the same person.
   - **First name** (`compare_first_names()`): exact → 1; a *direct* nickname
     (`BOB` ↔ `ROBERT`, from `nickname_table()`) → 0.95 (sibling diminutives like
     `LISA`/`BETH` are **not** equated); an initial vs a full name → weak; a
     Soundex sound-alike removes the penalty without asserting a match.
   - Both name similarities are **rescaled** so that unrelated names land near 0
     and actually *penalise* in scoring. Plain Jaro–Winkler rarely drops below
     ~0.5, so without this a clear mismatch reads as "half a match" and a nickname
     + gender agreement could override a different surname.

3. **Scoring with a surname-frequency adjustment.** Each field contributes
   `weight × (2·similarity − 1)`; a missing field is neutral. The surname weight
   is scaled *per organization* by how rare the surname is inside that org
   (`surname_weight()`), **asymmetrically**: an *agreement* is weighted by the
   commoner of the two sides (`pmin`, so a shared common surname on a
   family-dominated board carries almost no information and the first name /
   suffix / gender do the discriminating), while a *disagreement* is weighted by
   the rarer side (`pmax`, so a genuine surname mismatch keeps its full penalty
   and cannot be diluted away). See `default_weights()`.

4. **One-to-one matching.** Within each year pair, matches are resolved
   one-to-one (greedy, highest score first), so a record links to at most one
   record in another year.

5. **Closure into stable IDs.** Accepted links are closed into connected
   components under a hard invariant: **a person cluster holds at most one record
   per organization-year.** Links are added strongest-first; any link that would
   fuse two records from the same org-year is rejected and counted. Each cluster
   gets one deterministic `EMP_ID` (a content hash of its sorted members), so the
   id is stable across runs as long as the membership is.

## Usage

```r
# install.packages("devtools"); devtools::install_local("path/to/synthid")
library(synthid)

panel  <- readr::read_csv("PANEL-2019-2021-W-NAMES-TITLES.csv")
linked <- link_panel(panel)          # adds EMP_ID, EMP_N_RECORDS, EMP_N_YEARS
synthid_report(linked)               # run summary + diagnostics
```

Adapt to a differently-named panel by passing a modified column map:

```r
cols <- synthid_cols()
cols$org_id <- "ein9"
linked <- link_panel(panel, cols = cols, threshold = 8) # raise for max precision
```

### Expected input columns

| role         | default column   | required |
|--------------|------------------|----------|
| organization | `ein`            | yes      |
| tax year     | `taxyr`          | yes      |
| full name    | `name`           | yes      |
| name parts   | `first_name`, `middle_name`, `last_name`, `suffix`, `salutation` | used if present |
| gender       | `gender` (`F`/`M`/`U`) | used if present |
| title        | `title.standard` | used if present |

### Output columns

- `EMP_ID` — stable cross-year person identifier.
- `EMP_N_RECORDS` — records in the person's cluster.
- `EMP_N_YEARS` — distinct years the person is observed.
- `EMP_LINK_CONF` — (`method = "em"` only) the weakest posterior probability among
  the links holding the person's cluster together; a ready-made confidence input
  for a downstream panel model. `NA` for single-record persons.

## Match models

Two scoring methods produce the links:

- **`method = "weighted"` (default)** — the hand-set additive score described
  above, with a tuned threshold. Fast and deterministic.
- **`method = "em"`** — fits an **unsupervised** Fellegi–Sunter latent-class model
  (`fit_match_model()`, a self-contained EM equivalent to
  `reclin2::problink_em()`) to the candidate comparison vectors, learning the
  agreement probabilities among matches (`m`) and non-matches (`u`) with **no
  labels**, and links by the learned posterior match probability. It also emits
  `EMP_LINK_CONF`.

```r
linked_em <- link_panel(panel, method = "em")
fs_weights(attr(linked_em, "synthid")$model)   # learned agreement/disagreement weights
```

The EM model is also a **validation of the hand-set weights**: on the 2019–2021
panel the two methods agree on **99.85%** of all candidate accept/reject
decisions. The learned weights confirm the large first/last-name weights (~6 bits
of agreement each), and suggest two refinements the hand weights understate:
middle-name agreement is more informative than assumed, and *disagreements*
(especially first name, suffix, gender) deserve much stronger penalties than a
symmetric ±weight gives — which is what the rescaled comparators approximate. The
EM posterior is sharply bimodal (≈0 or ≈1) and well-calibrated, making
`EMP_LINK_CONF` a meaningful confidence signal.

The lower-level pieces are exported for custom pipelines:
`candidate_comparisons()` (per-field comparison vectors), `fit_match_model()`
(`method = "em"` or `"logistic"`), `predict_match()` (calibrated `p_match`), and
`fs_weights()`.

## Tuning

- `threshold` — minimum match score to link two records (default `7`). Chosen by
  tuning against a labeled slice (`dev/tune_threshold.R`): on the 2019–2021 panel
  it gives pair-level precision 0.98 / recall 1.00 overall and precision 0.985 /
  recall 0.955 on the family-board slice, with post-one-to-one precision ~0.999.
  Raise toward 8–8.5 for maximum precision (at some cost to family-board recall),
  lower for higher recall.
- `weights` — override `default_weights()` to reweight fields.

## Status and roadmap

This is **v0.1**: a runnable linkage layer that emits stable IDs, with tuned
comparators, a learned match model, and a calibrated confidence signal. Planned
next:

- Learned weights via an unsupervised Fellegi–Sunter EM are **implemented**
  (`method = "em"`, `fit_match_model()`), emitting a calibrated `p_match` /
  `EMP_LINK_CONF` for the downstream HMM. A supervised `"logistic"` option exists
  but the silver labels are perfectly separable (MLE diverges); it needs
  regularization or a noisier gold set to be useful.
- Nickname (Bob↔Robert) and phonetic first-name features are implemented
  (`compare_first_names()`); the nickname dictionary can still be extended (e.g.
  `ANTON`↔`TONY`), and a NYSIIS / Double Metaphone backend would beat Soundex if
  the `phonics` package is added. A stronger suffix veto for generational splits
  is still open.
- A precision/recall harness with a dedicated family-board slice exists in
  `dev/tune_threshold.R` (silver labels from an independent policy). Next: replace
  silver labels with a hand-verified gold set and add a held-out (org-disjoint)
  split.
- Performance: the compound-name comparator is currently R-level; it will be
  vectorized for panels much larger than the 500-org dev set.
- A secondary org-name blocking pass for EINs that change on merger.
