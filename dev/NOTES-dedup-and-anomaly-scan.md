# Pre-match de-duplication and the post-hoc anomaly scan

Investigation notes behind two questions: whether to de-duplicate records before
matching (**answer: no -- it was built, measured, and reverted**, see §1), and a
new false-merge / entity-defect flagger (**shipped** as `flag_anomalies()`).

All figures below are measured, not estimated. Sources:

* `dev/test_panels_1k/panel_{01..05}.rds` -- 804,410 raw Part VII records,
  5,000 EINs x 15 years (2010-2024).
* `dev/data/linked_full_panel_par_1000eins.rds` -- a real `link_panel()` output:
  159,390 records, 32,988 persons, 1,000 orgs, 2010-2024.
* `dev/data/linked_slice_enriched.rds` -- 1,000 EINs x 3 years with parsed name
  components, used for timing and the score-equivalence proof.

Where a figure comes from the raw panels it is quoted **after** replicating the
filing-resolution step in `dev/01_preprocess_year.R` (see §1), because that step
removes most of what naively looks like duplication.

---

## 1. There is no de-duplication before matching, and the right key is (EIN, name, title)

`prepare_panel()` keeps every input row. `build_id_string()` goes the other way:
where two rows produce an identical `EIN + year + name + title` string it appends
a `-D{k}` occurrence suffix so they stay distinct. `link_year_pairs()` then loops
over all `choose(Y, 2)` year pairs and materialises the full `n_i x n_j` cross
product per org.

The only existing accelerator is `memoize_comparator()`, which caches string
comparator results by value-pair. It removes the string work but not pair
materialisation, comparator dispatch, memo-key construction, or score arithmetic.

Profile of `link_panel()`, 300 EINs x 3 years, 49.5 s wall:

| stage | share of runtime |
| --- | --- |
| `reclin2::compare_pairs` -> comparators | 94% |
| ... `preprocess_name` / `trimws` / regex `sub` | 46% |
| ... `stringdist::stringdist` | 31% |
| clustering, id minting, everything else | ~6% |

### The signature is exactly the score's input set

`default_weights()` is `last_name, first_name, middle_name, suffix, salutation,
gender, title.standard` -- **title is a match feature** (weight 0.5). The only
other input to the score is `surname_weight()`, a function of org + full name +
surname. So within an org the score is a pure function of

    (name, parsed components, title.standard)

and the parsed components are deterministic from the name. That is
`(EIN, person name, title)`.

**Verified empirically, not by inspection.** Computing one signature x signature
block per org and expanding it back recovered **all 75,309** year-pair scores with
**max absolute difference exactly 0.0**, and `surname_weight` had zero spread
inside every signature group. This is a pure accelerator on the same footing as
the memoizer: identical edges, clusters and `EMP_ID`s.

### Size of the win, and why it only pays at long windows

Across all five panels (804,410 records, 15 years): 275,990 distinct signatures =
**34.3%** of records; candidate pairs **100,737,976 -> 14,927,598 (6.7x fewer)**.

The gain is a function of window length, because the deduped block is computed
once per org while the current cost grows as `choose(Y, 2)`:

| window | pairs now | deduped | ratio | signatures |
| --- | --- | --- | --- | --- |
| 2 years | 183,707 | 158,145 | 1.2x | 64% |
| 3 years | 540,899 | 243,212 | 2.2x | 53% |
| 5 years | 1,745,550 | 488,394 | 3.6x | 45% |
| 10 years | 7,604,225 | 1,304,369 | 5.8x | 37% |
| 15 years | 17,658,518 | 2,537,320 | **7.0x** | 34% |

### It was built, and it does not work. Reverted.

The cache was implemented (signature-pair store keyed by integer signature ids,
misses routed through the identical reclin2 path) and verified byte-identical:
same `EMP_ID`, `EMP_ANCHOR`, edge count, rejected-edge count, and an identical
`candidate_scores()` frame on a 10,241-record panel. Then it was timed against
real 15-year panel structure and showed **no gain at any window length**:

| window | dedup off | dedup on | speed-up |
| --- | --- | --- | --- |
| 3 yr | 3.3 s | 3.2 s | 1.04x |
| 6 yr | 6.5 s | 6.6 s | 0.98x |
| 9 yr | 11.8 s | 12.7 s | 0.93x |
| 12 yr | 17.8 s | 19.5 s | 0.91x |
| 15 yr | 27.4 s | 26.9 s | 1.02x |

Crossing it with the existing memoizer (18 orgs, 15 years, 2,841 records) shows
why:

| memoize | dedup | seconds | vs neither |
| --- | --- | --- | --- |
| off | off | 108.9 | 1.00x |
| **on** | **off** | **8.9** | **12.24x** |
| off | on | 33.8 | 3.22x |
| on | on | 17.3 | 6.29x |

De-duplication does remove real work -- 3.22x on its own. But
`memoize_comparator()` removes strictly more of the same work (12.24x), and
running both is **0.51x**, roughly twice as slow as the memoizer alone.

The reason is structural, not an implementation defect. `memoize_comparator()`
caches at the **field-value pair** level; the signature cache caches at the
**signature pair** level, which is strictly coarser. Two records that differ only
in title carry different signatures but identical surname and given-name pairs, so
the memoizer hits where the signature cache misses -- never the reverse. Every
signature-pair hit is necessarily a set of field-value-pair hits the memoizer had
already served. The signature cache can therefore only ever add key construction
and lookup overhead on top of a cache that has already collected the winnings.

**The 6.7x figure above counts pairs, not cost.** That was the error in the
original analysis: the memoizer had already collapsed the per-pair cost of
precisely the repeated pairs the signature cache targets, so the pair-count ratio
never converted into a time ratio. A pair-count projection is only a speed-up
projection when per-pair cost is constant, and here it is not.

Recorded so the idea is not re-derived from the same (correct, but insufficient)
pair-count evidence. If the linker's cost model ever changes -- a vectorised
comparator that makes the memoizer's per-call overhead dominant, or a move off
reclin2 -- the question is worth reopening; nothing else about it will be.

---

## 2. Same name + title twice in one filing: 0.074%, and nothing to merge

Raw, there are 8,682 such groups. **97% are multiple filings for one EIN-year**
(original plus amendment), which `dev/01_preprocess_year.R` already resolves:
783 org-years affected, 1.10% of rows dropped as superseded.

Genuine residual, inside one resolved filing at the `(OBJECTID, TABLE_ID)` grain:
**287 groups / 592 records = 0.074% of the panel**, touching 196 of 74,337
org-years.

| | groups | share |
| --- | --- | --- |
| identical on every pay / hours / position field | 246 | 86% |
| differ only in pay | 6 | 2% |
| differ in position flags and/or hours | 35 | 12% |

**The split-reporting hypothesis does not occur -- 0 cases.** Searched
specifically for "one line carries org comp, another carries related-org comp,
totals agree". The six pay-differing groups are simply different numbers
(`TODD LENARSKI / REPRESENTATIVE` 75 vs 150; `BILL MARLETTE / TREASURER`
1,749,053 vs 5,903,232). There is no merge to perform.

### Decision: keep both records; do not drop, do not merge

1. 0.074% of the panel -- there is no runtime argument.
2. `PERSON_YEAR_ID` is 1:1 with panel rows by construction
   (`dev/01_preprocess_year.R`); dropping a row breaks that back-link.
3. The merge pattern that would justify collapsing them is absent from the data.

Flag them instead (`dup_line_in_filing`). If a drop policy is ever wanted it
belongs beside the filing resolution in `01_preprocess_year.R`, not inside
`link_panel()`.

For the de-dup design this needs no special handling: duplicates collapse to one
signature but expand back to distinct records, so `greedy_one_to_one()` and the
one-record-per-org-year guard in `resolve_clusters()` still see them individually.

---

## 3. Institutional listees enter the person matcher as pseudo-people

1.39% of records (11,203) have `F9_07_COMP_DTK_NAME_ORG_L1` populated with
`F9_07_COMP_DTK_NAME_PERS` empty. Never both.

`dtk.name` coalesces the two, so these records are parsed into first/last name
components and linked exactly like people. 62% of the resulting entities are
multi-year; **40 genuine institutions span 8+ years** -- `GENWORTH LIFE INSURANCE
COMPANY`, `U S BANK NA`, `REGIONS BANK`, `UNION BANK AND TRUST CO`, all 15/15.
Each becomes a fake long-tenure, high-degree node: precisely the node type that
distorts centrality and path-length statistics.

The multi-seat scenario (one institution holding several seats, delegates
unnamed) is real but rare: **8 of 11,195**, e.g. `CENTURY 21 TAWAS REALTY` as
DIRECTOR *and* SECRETARY/TREASURER in one filing.

**`NAME_ORG_L1` cannot be used as an "is an organization" switch.** Only 3% of its
distinct values carry a corporate token; **91% are 2-3-token personal names that
filers put in the wrong field** (`SUSAN MCCOY`, `DON G WELLONS`,
`DIANA GOLDBERG`). Two separate flags are therefore needed:

* `name_from_org_field` -- expansive and exact: `name` populated while `name.raw`
  is blank. `name.raw` already carries the literal `F9_07_COMP_DTK_NAME_PERS`
  (`dev/01_preprocess_year.R`), so this is a data check with no heuristic.
* `org_as_person` -- the subset whose name also carries a corporate token, i.e.
  very likely a genuine institution being modelled as a person.

Scale: ~0.31% of records, but concentrated in structurally important positions.

---

## 4. Two failure modes, only one of which collapses network components

* **Co-present homonyms** -- two people of the same name listed in the same org in
  the same year. The linker produces the *correct number* of `EMP_ID`s and threads
  them arbitrarily between the two people. This is an attribution swap: individual
  trajectories are corrupted, but component structure, node count and degree
  distribution survive.
* **Sequential homonyms** -- person A holds a seat 2010-2014, a different person B
  of the same name holds it 2018-2024, one record per year throughout. Greedy
  one-to-one links them into **one** `EMP_ID`. This is a genuine component merge,
  with the distance-shortening / betweenness-inflating / density-invisible
  signature that makes it hard to detect from network statistics alone.

### `flag_links()` is structurally blind to both

`flag_links()` flags a cluster only when its member *names disagree* -- first
names differ, last names differ, or raw-name divergence exceeds a threshold. A
merge of two people with byte-identical names scores zero on all three criteria
and is never flagged. **The existing flagger is a false-split detector; there is
currently no false-merge detector.** That gap is what `flag_anomalies()` fills.

---

## 5. Detection signals, with measured base rates

### Homonyms need no salary and are deterministic

Same name appearing twice in one org-year is proof of two people:
1,442 org-year-name groups, 862 distinct (org, name), **7,419 records = 0.93% of
the panel**; 80% have differing titles. Highest-value flag, and it is a `.N > 1`
count.

### Gap + pay-level shift is tight

Of 10,824 paid (org, name) histories with >= 4 observations, 1,854 have a gap;
**153 (1.41%) have a gap >= 2 years *and* a > 100% pay-level shift across it.**

### Bimodality alone is too noisy to use

13% of paid histories contain a >= 1.0 log-gap (a 2.7x step) somewhere in the
sorted series. The extreme cases (`JOSEPH GWIN` $81 -> $116,071) are
overwhelmingly volunteer-board-member-becomes-paid-staff, or a reporting-basis
change -- not two people. Use only conjoined with a gap or a role-class change.

Compensation is available downstream: `tot.comp` is carried into the panel by
`dev/01_preprocess_year.R`, so all pay checks run post-match with no change to
the matcher.

### Structure of the real linked panel

159,390 records, 32,988 persons, 1,000 orgs, 2010-2024.

Invariants -- **both hold, zero violations**. Same `EMP_ID` twice in one year: 0.
`EMP_ID` spanning more than one org: 0. These belong in the test suite as
assertions, not in the flag table.

Temporal: singletons 19.6%; median tenure 3 years; 5.0% span all 15. Gapped
careers 2,172 = **8.2% of multi-year persons**. Spells: 2,004 persons with two,
155 with three, 13 with four. The gap rate is flat across org size (5.8% at <= 20
persons to 8.9% at 101-200), so it is not an artifact of blocking blow-up in
large orgs.

---

## 6. The flag set -- `flag_anomalies()`

Implemented in `R/flag_anomalies.R`, tested in
`tests/testthat/test-flag-anomalies.R`. Returns one row per person, worst-first,
with a primary `category` and a `merge_risk` score; `anomaly_review_queue()`
extracts the over-link categories (entity defects and duplicate lines are ingest
items, not linkage decisions, and are excluded unless asked for).

Category priority follows `flag_links()`: a mechanical explanation outranks the
residual "two people?" reading. In particular `duplicate_line` is tested *before*
`homonym_copresent`, because two records sharing organization, year, name and
title inside one filing are a duplicated Part VII line -- the benign cause of what
would otherwise read as two same-named people.

Every flag whose input column is absent degrades to `NA` rather than to `FALSE`,
so a missing `tot.comp` cannot be mistaken for "no pay anomalies". `name.raw` is
what makes `name_from_org_field` exact; without it the org-field check falls back
to name shape and says so in the printed summary.

**Denominators.** The scan is a per-person table, so every count in its
`flag_counts` summary is a count of *persons*. The §5 investigation figures below
were computed on *records*, because at that point there was no linked panel to
count persons in. The two are not interchangeable -- an institution listed for 15
years is 15 records but one person -- so both are given.

| flag | rule | investigation | scan output |
| --- | --- | --- | --- |
| `homonym_copresent` | (org, name) appears >= 2x in some year | 0.93% of records | 1.51% of persons |
| `gap_return` | gap >= 3 yrs with >= 2 yrs present each side | 1.96% of persons | 1.78% |
| `gap_long` | gap >= 5 yrs | 1.06% of persons | 1.00% |
| `gap_pay_shift` | gap >= 2 **and** pay level shift > 100% | 1.41% of paid | 0.08% of persons |
| `sporadic` | span >= 6 but present in < 40% of it | 0.41% of persons | 0.50% |
| `name_from_org_field` | `name` populated, `name.raw` blank | 1.39% of records | 0.73% of persons |
| `org_as_person` | ... and the name carries a corporate token | ~0.31% of records | 0.01% of persons |
| `placeholder_name` | VACANT / SEE ATTACHED / NONE / BLANK / TBD | 110 records | 2 persons |
| `single_token_name` | one-token person name | 518 records (0.06%) | 5 persons |
| `dup_line_in_filing` | the 287 groups from §2 | 0.074% of records | 21 persons |

The union of the merge-candidate flags lands around 3-4% of persons -- small
enough to review by hand, and aimed squarely at the error class the existing
diagnostics cannot see.

### Validation run

`flag_anomalies()` over a linked 220-org x 15-year panel (33,062 records ->
7,367 persons), scan-output column above:

* **Invariant violations: 0.** Neither `dup_id_in_year` nor `id_spans_orgs`
  fired, matching the direct check on the production linked panel in §5.
* **Merge-risk queue: 241 persons = 3.27%** -- inside the predicted 3-4% band.
* Category split: `sequential_homonym_gap` 121, `homonym_copresent` 88,
  `org_field_name` 52, `intermittent` 23, `duplicate_line` 21,
  `sequential_homonym_pay` 6, `entity_not_person` 3, `name_shape` 3.
* The person-level temporal flags reproduce the §5 base rates closely
  (`gap_return` 1.78 vs 1.96, `gap_long` 1.00 vs 1.06, `sporadic` 0.50 vs 0.41,
  `dup_line_in_filing` 21 persons vs 287 groups over ~24x more records).

Two caveats on this run. It draws 220 of panel_01's 999 organizations, and both
homonyms and institutional listees cluster in large organizations, so their rates
are sampling-sensitive at this size. And the names were parsed crudely
(peopleparser is not installed in this environment), which changes cluster
membership at the margin; it does not affect `homonym_copresent`,
`name_from_org_field` or `dup_line_in_filing`, which read the raw name fields
directly. Re-run against a peopleparser-parsed panel before quoting these as
production rates.

`gap_pay_shift` is the one flag that fires far below its investigation rate
(0.08% of persons against 1.41% of paid histories). That is expected rather than
wrong: the investigation denominator was paid `(org, name)` histories with >= 4
observations, and only a small minority of persons in a full panel are paid at
all -- 87% of Part VII records carry zero organization compensation (§5). The
flag is narrow by construction.

Two further flags are worth adding once evaluated against a parsed 15-year panel:
`role_discontinuity` (role class jumps across a gap, e.g. BOARD -> CEO; pairs
naturally with `gap_return`), and a false-split counterpart looking for two
singletons in one org with identical names in adjacent years.
