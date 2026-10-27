# Plan: board-interlock data transformations and descriptive metrics

Status: draft, 2026-10-27. Built from the Raleigh-Durham-Cary (CSA 450) prototype
in `dev/partvii-panel/06b`-`06e`.

## Why

synthid ends at linkage: `link_panel()` gives each person a within-org `EMP_ID`,
`link_cross_org()` gives a cross-org `XORG_ID`. Every network analysis after
that was hand-written in the prototype scripts, and the same steps came up each
time:

1. pick board seats out of the linked panel,
2. collapse organizations that are one governance team filed as several legal
   entities (meta-orgs),
3. project the person-organization data into an organization network (shared
   board members) or a people network (shared board service),
4. filter by tie strength, and
5. describe how ties fall within and across groups (NTEE sector, CBSA, CSA).

These steps decided whether the results meant anything: without meta-org
collapse, 21% of 2+ shared-member org ties were one organization's own
entities; without a stricter person match, the "top bridges" were JOHN SMITH
chains. They belong in the package, with the defaults the prototype found.

## What the prototype found (CSA 450)

- **Geography sorts ties more than industry.** 78% of 2+ shared-member org ties
  stay within one CBSA (48% expected at random; assortativity 0.56) vs 31% within
  one MAJ12 group (17% expected; 0.16).
- **Most board ties cross sectors.** In Raleigh-Cary, the share of a sector's
  ties that stay inside it runs from 2% (international) to 20% (human services).
- **NTEE blocking loses most cross-org matches**: 74% of surname + CBSA matches
  cross NTEE groups (step 6b).
- **24% of organizations have NTEE "UNDEFINED"**, which a first-letter rule
  files under U (science & technology).
- **Meta-orgs**: 75 meta-orgs covering 212 entities (shared >= 3 board members,
  Jaccard >= 0.5); they hold 21% of 2+ shared-member org ties but 2% of 1+ ties.
  The rule misses organization + foundation pairs whose boards differ in size
  over 15 years (NC Advocates for Justice and its foundation; NCTA and its
  education foundation; WakeMed and its group return).
- **People networks need a stricter person match than org networks.** At score
  7 a bare common first + last name scores ~9 and chains (one "Michael Smith"
  on 44 boards). At score >= 10 the top bridges are distinctive names on 10-21
  boards.
- **Projections are hairballs unfiltered.** Every board is a clique in the
  people projection (14,875 people, mean degree 36). The k-core of the org
  projection, people tied by 2+ shared boards, and within/between-sector
  nesting made them readable.

## Proposed functions

All take and return plain data frames (edge lists, membership tables), so they
work without a graph library; igraph stays in Suggests and is used only where
named. Column names follow `synthid_cols()`.

### 1. Board seats

`board_seats(linked, xorg = NULL, roles = "BOARD", cols = synthid_cols())`
-> one row per person x organization: `org_id`, `person_id`, `first_year`,
`last_year`, `years` (list), `titles` (list).
- `person_id` is `XORG_ID` when `xorg` (from `link_cross_org()`/`stamp_xorg()`)
  is given, else `EMP_ID`.
- `roles` uses `title_role()`; `"ANY"` keeps every seat. Default
  `c("BOARD", "OFFICER")`, with a `seat_type` column (board / executive) so an
  executive of one organization sitting on another's board becomes an
  executive-board tie that analyses can keep or drop. In CSA 450, 18% of
  cross-org persons mix board and non-board seats (mostly executive directors,
  CEOs and CFOs); the 06c-06f prototype used board seats only, so it drops
  those ties. Staff seats stay out of interlock networks (a program director at
  two organizations is usually a job change); matching still includes everyone,
  which keeps career-mobility analysis possible.

### 2. Meta-orgs

`detect_meta_orgs(seats, min_shared = 3, jaccard = 0.5, overlap = NULL, by_year = FALSE)`
-> `org_id`, `meta_id`, `meta_size`, plus the pair table with `shared`,
`jaccard`, `overlap` (shared / smaller board) for review.
- A meta-org is a connected set of organization pairs passing the rule; named
  by its member with the largest board.
- `by_year = TRUE` compares boards year by year and requires the rule in most
  co-filing years. This should catch organization + foundation pairs that the
  pooled 15-year Jaccard misses. Test it against the three missed pairs above.

`collapse_meta_orgs(seats, meta)` -> seats keyed by `meta_id`, years and
titles unioned.

`meta_org_share(seats, meta)` -> share of 1+ and 2+ org ties, and of
interlockers, that sit inside meta-orgs (the "how much do they inflate" table).

### 3. Projections

`org_projection(seats, min_shared = 1, same_year = FALSE)`
-> `org_a`, `org_b`, `shared` (distinct people), `shared_years`.

`person_projection(seats, min_shared = 1, same_year = TRUE, interlockers_only = TRUE)`
-> `person_a`, `person_b`, `shared` (boards served together).
- `same_year = TRUE` counts a board only if both served in a common year.
- `interlockers_only` keeps people on 2+ boards; `min_shared = 2` is the
  "inner circle" network.
- Guard: warn when the projection will exceed a row budget (a 100-member board
  is 4,950 pairs) and report the largest boards.

`network_core(edges, max_nodes = 160, k = NULL)` -> the largest connected
group, then the smallest k-core with at most `max_nodes` (or a given `k`);
returns edges plus `attr(, "k")` and the size at every k. (igraph)

### 4. Group mixing and the E-I index

`ei_index(edges, groups, level = c("network", "group", "node"), weight = NULL,
rescale = FALSE, permutations = 0, seed = NULL)`

Krackhardt and Stern (1988): `EI = (E - I) / (E + I)`, where E counts ties
between groups and I ties within. -1 means all ties are internal, +1 all
external.
- `level = "group"`: per group, from the ties its members have;
  `"node"`: per organization or person; `"network"`: one value.
- `rescale = TRUE`: replaces counts with densities (ties over possible ties
  within / between), so large groups are not internal by size alone (as in
  egor's `EI(rescale = TRUE)`).
- `permutations > 0`: shuffles group labels to give the expected EI and a
  p-value. A group of sector-crossers reads very differently once you know
  random mixing would give, say, -0.1.
- `weight`: uses tie strength (`shared`) instead of counts.
- Also returns `I`, `E`, `within_share = I / (I + E) = (1 - EI) / 2` (the
  "within" share on the prototype's sector circles) and the expected values.
- Units are anything in `groups`: NTEE letter, MAJ10/MAJ12, CBSA, CSA, a
  meta-org, or a community. CBSA vs CSA is one call each with the same edges.
- netseg (`ei()`, plus `orwg()`, `ssi()`, `freeman()`) and egor (`EI()`)
  implement it. Write our own on data frames, without those dependencies, and
  check it against `netseg::ei()` in tests.

`group_mixing(edges, groups, sizes = NULL)` -> the group x group table:
`ties`, `possible`, `density` (per 1,000 pairs), within and between. This is
the data behind the 26-node circle graph and the nested sector graphs.

`assortativity_by(edges, groups)` -> nominal assortativity and modularity of a
partition (igraph), with the random-wiring expectation for the within share.

### 5. People

`bridge_scores(seats, org_attrs, by = c("maj10", "cbsa"))` -> per person:
boards, distinct groups spanned per `by`, `sole_links` (org pairs only this
person links), brokerage (betweenness in the person-organization graph,
igraph), and E-I over the boards they link.

`inner_circle(seats, min_shared = 2, method = "louvain", min_group = 6)` ->
membership (`person_id`, `group`) and a group table labeled by the boards most
members share.

`name_risk(xorg, profiles)` -> per cross-org person: surname rarity, whether a
middle initial backed the match, organizations per person. Bridge and
inner-circle results should report it, and the docs should recommend a
stricter `link_cross_org(threshold = 10)` for people-level analysis.

### 6. NTEE and geography helpers

- `ntee_major(ntee)`: the letter, with `"UNDEFINED"` and `"Z99"` -> `"Z"`.
- `ntee_maj10(ntee)`, `ntee_maj12(ntee)`: NCCS groupings (universities and
  hospitals split out in MAJ12).
- `ntee_division(ntee)`: A2, A5, ...; the subsector for single-letter sectors.
- A CBSA -> CSA crosswalk as package data (from the 2023 delineation file), so
  `groups = cbsa_to_csa(cbsa)` works.

### Out of scope for the package (vignette or dev only)

The nested circle graph, the bridge dot chart and the board diagrams use
ggraph, graphlayouts and ggrepel; keep the plotting in the vignette. If a plot
helper is wanted later, `plot_group_mixing()` on `group_mixing()` output is the
first candidate.

## Order of work

1. NTEE and geography helpers, `board_seats()`, `org_projection()`,
   `person_projection()`. Tests on the `make_panel()` fixtures plus a small
   hand-built seats table with a known meta-org and a known bridge.
2. `ei_index()` and `group_mixing()`. Tests: hand-computed EI on a 6-node
   example, rescaled EI, agreement with `netseg::ei()` (skip if not
   installed), and a permutation expectation near the analytic one.
3. `detect_meta_orgs()`, `collapse_meta_orgs()`, `meta_org_share()`, including
   `by_year`, checked against the 06c meta-org list and the three missed pairs.
4. `network_core()`, `assortativity_by()`, `bridge_scores()`,
   `inner_circle()`, `name_risk()`.
5. Re-run 06c-06e on the new functions and confirm the same numbers: 75
   meta-orgs, 78% within-CBSA, the sector within shares.
6. The vignette (below).

## Vignette: board interlocks

Turn the CSA 450 analysis into `vignettes/board-interlocks.Rmd`: "From Part VII
records to board-interlock networks". Outline:

1. Board seats from a linked panel; why role filtering matters.
2. Meta-orgs: detect, review, collapse; how much they inflate ties.
3. Two projections: organizations sharing board members, and people sharing
   boards; filtering by tie strength and the k-core.
4. Within vs between groups: E-I index by MAJ10, CBSA and CSA; geography vs
   industry; the 25-group circle and the nested sector graph.
5. People: bridges and the inner circle, with the stricter person match and
   the name-risk check.
6. Caveats: NTEE UNDEFINED, pooled years vs single-year snapshots, false
   merges of common names, national associations with a local address.

Data: vignettes cannot read the full panel. Ship a small extract (one CBSA,
organization-level edges and seats with hashed person ids), or simulate a panel
with the same structure. Do not print individual names in the vignette, even
though they come from public filings: show organizations, and person ids or
synthetic names. Add it to the "Articles" menu in `_pkgdown.yml`.

## Linkage reliability: false matches and homonyms

Two kinds of false cross-org link, with different fixes:

- **Variant false positives**: two different names clear the threshold
  (MENDEZ / MENDOZA, PHELPS / PHILLIPS). The scorer can see these; they are a
  threshold and weighting problem.
- **Homonyms**: two different people with the same name. The scorer cannot see
  these: an identical full name with no middle initial scores ~9-10 whoever it
  is. They are a base-rate problem, and they hit networks hardest, because a
  merged homonym looks exactly like a bridge.

### Pilot (06g-name-risk.R, CSA 450, matches at score >= 7)

- 67% of matched pairs are identical full names with no middle initial to
  confirm them; 28% are name variants (median score 8.6); 4% are confirmed by
  agreeing middle initials; 0.6% have conflicting middle initials and still
  pass.
- Name commonness: lambda = records with that full name expected by chance in
  the CSA, `n * p(first) * p(last)`. Cross-org board members average 2.5
  boards for rare names (lambda < 0.01), 3.0 for lambda 0.1-1, 4.6 for lambda
  1-10, and 21 for the 9 very common names (lambda >= 10). Rare names show the
  true distribution; the excess for common names estimates homonym merging.
- 349 of 14,916 cross-org board members (2.3%) are "ambiguous": one identical
  name, lambda >= 1, no middle-initial confirmation. 2.7% of organization ties
  (1+ shared members) and 2.5% of 2+ ties rest on them alone.
- Splitting them back into one person per organization moves most network
  measures about as much as splitting the same number of other people with the
  same board counts (ratio 0.7-1.5), except the CBSA E-I index, which moves 15
  times as much: homonym merges disproportionately create cross-metro ties.
  Bridge rankings at score 7 were dominated by them (06e).

### How to measure it

1. **Clerical review sample.** Stratify matched pairs by score band (7-8, 8-9,
   9-10, 10+) x match type (identical name, variant) x name commonness, about
   50 pairs per cell, two reviewers. Evidence: the two organizations' locations,
   sectors, years and titles, shared co-members, and web board lists and bios
   where the seats are current. Gives precision per cell with confidence
   intervals, weighted up to all pairs. Bios cover only current boards; that
   is fine for a validation sample.
2. **Known non-matches from the data.** Two distinct people with the same name
   in the same organization and year (the within-org linker keeps co-present
   records apart) are ground-truth homonyms. Their frequency, by name
   commonness, calibrates the homonym base rate. `flag_anomalies()` already
   finds co-present homonyms.
3. **Name-frequency model.** Turn lambda into P(same person | identical name,
   area). Better denominators than the panel itself: Census surname
   frequencies, SSA first-name frequencies, and local name counts from public
   voter files (counts only, not record linkage).
4. **Excess-boards decomposition.** Model boards per person on name
   commonness; the rare-name curve is the counterfactual. The excess for common
   names, summed, estimates the number of false merges.
5. **Frequency-adjusted Fellegi-Sunter.** `fit_match_model()` with
   frequency-based u-probabilities for the full name (not just the surname),
   so an exact match on JOHN SMITH earns less than one on CREIGHTON BLACKWELL.

### How to split identical-name clusters without bios

Borrow from author-name disambiguation, where co-authors play the part of
co-board members:

- **Co-membership.** Within a common-name cluster, keep two seats together
  only if their boards share another member, directly or through a short path
  that does not run through this person. A real person's boards tend to
  connect; a homonym merge joins unconnected parts.
- **Context fields.** Same or adjacent county or ZIP, same sector, overlapping
  or consecutive years, compatible titles and compensation, Schedule R related
  organizations. Each one is weak; require two or more for an ambiguous name.
- **Plausibility.** Cap concurrent boards per year; flag clusters whose boards
  per year far exceed the rare-name distribution.
- **Hard stops.** Conflicting middle initials or suffixes never merge.
- **External sources with history.** NC Secretary of State annual reports list
  nonprofit officers by year (back filings included), and could confirm a
  sample.
- **Bounds instead of a single answer.** Report network measures with
  ambiguous persons merged (upper bound) and split (lower bound), or weight
  each cross-org link by P(same person).

### Composite names: what pooling within-org variants buys

Profiles already pool everything seen for a within-org person: every surname
(with a blocking key per surname and per compound component), every first name
with its nickname roots, middle initials (from initials or full middle names)
and suffixes; the scorer takes the best match across the sets.

Test (06i-variant-test.R, CSA 450): rebuild each profile from one form (modal
surname, most formal first name, one initial, one suffix) and rerun blocking
and scoring. Of 89,237 matches at score >= 7, 98.5% survive; 538 (0.6%) lose
their shared surname key and 764 (0.9%) fall below 7. Pooling never removed a
match. The pooled-only matches are mixed:

- **real variants recovered**: typos (KACIE FORCE / FORE), nicknames (BETTIE /
  BETH), spacing (MARYANN / MARY ANN), married names;
- **noise imported**: parse errors pooled as surnames (D REID WILSON gives the
  person a REID surname and matches DAVID E REID); within-org false merges (an
  EMP_ID holding both DAVID HENDERSON and DAVID JOHNSON matches another David
  Henderson); loose spellings (SUSAN HOLLEY vs SUSAN HILL); nickname roots that
  do not meet (JOE/JOSEPH vs JOSH).

Changes to the composite, roughly in order of value:

1. **Attest variants.** Keep a count of filings and years per variant. Block
   and score on variants seen in 2+ filings, plus patterned changes (a
   hyphenated or added surname, a nickname of the formal name); give a match
   that rests only on a once-seen variant a penalty or a review flag.
2. **Quarantine parse-suspect variants.** A "surname" that appears as the
   person's middle name in other filings, or a first token that is an
   initial, does not get a blocking key.
3. **Flag suspect within-org merges** whose surname variants are unrelated
   (not compound, not a spelling variant) and keep their variants out of
   cross-org matching; they are within-org linkage errors.
4. **Keep full middle names, not only initials.** Agreement on a full middle
   name is much stronger evidence than on an initial, and disagreement
   (ANNE vs ALICE) is a real conflict where the initials match.
5. **Do not impute nicknames into the sets.** `compare_first_names()` already
   handles nicknames through roots; adding imputed forms would only widen
   matching. Imputing a middle initial from a middle name is already done.

### False negatives beyond pooling

The variant test only counts matches that pooling rescues. Two more measures:

- **The review's 5-7 band** estimates true matches below the threshold.
- **A seeded perturbation test.** Take near-certain matches (rare full name,
  agreeing middle initials, score >= 11), alter one side the way names vary in
  the filings (drop the initial, use a nickname, a married or hyphenated
  surname, a one-letter typo, swap first and last), and measure how often
  blocking and scoring still find the pair. The rates of each kind of
  variation inside organizations (06h "name variants": 3.9% drop a middle
  initial; 25% of people who ever show an initial also appear without it; 16%
  are listed under a nickname; 3.2% change surname) weight the result into an
  overall miss rate.

### Seeded test results (06j-seeded-test.R, CSA 450, 2,000 seeds)

Seeds: rare identical full names scoring >= 11. Side B rewritten one way at a
time; recall = still in a shared block and scoring >= 7 (>= 10).

| Change to side B | In a shared block | Recall at 7 | Recall at 10 |
|---|---|---|---|
| none (control) | 100% | 100% | 100% |
| middle initial dropped | 100% | 100% | 94% |
| different middle initial | 100% | 100% | 62% |
| nickname (table names only) | 100% | 100% | 100% |
| first name reduced to initial | 100% | 100% | 4% |
| typo in first name | 100% | 98% | 22% |
| typo in surname | 47% | 47% | 29% |
| typo in surname's first letter | 16% | 16% | 15% |
| hyphenated married surname | 100% | 100% | 100% |
| surname replaced (married) | 0% | 0% | 0% |
| first and last swapped | 0% | 0% | 0% |

Weighted by how often each change appears within organizations, roughly 6%
of true cross-org pairs are missed, mostly from swapped order, surname
replacement and surname typos. Rough: the rates are per record, not per pair.

What it shows:

- **Blocking, not scoring, loses surname typos.** The scorer still rates a
  typo'd surname >= 7 every time, but the phonetic key changes in half the
  cases (all of them when the first letter changes). Fix: a second blocking
  key that tolerates a typo, e.g. (unit, first-name root, first two letters
  of the surname) or a second phonetic code, plus a token-order-free key
  (both name tokens, sorted) that also catches swapped order.
- **Score 7 is permissive on initials.** A conflicting middle initial still
  scores >= 7 every time (median 10.6), and a first name reduced to an
  initial scores >= 7 every time (median 7.8): J SMITH matches JOHN SMITH.
  These are false-positive risks; make a middle-initial conflict a strong
  negative and do not let an initial-only first name clear 7 without other
  evidence. Part of threshold tuning (step 7).
- **The nickname table is small** (70 formal names); only 112 of 2,000 seeds
  had a formal name in it. 16% of records use a nickname form. Extend it with
  a standard nickname list (about 1,000 names).
- **Married names that replace the surname cannot be found by name.** Only a
  separate rule could: same first name and middle initial in the same area,
  a different surname, and corroboration (shared co-members or the same
  organization family). Treat it as a known gap until then.

### Name checks and the hard-case library (06k-06m, CSA 450)

**Surname relation check** (06k). Classes: same, particle (VAN, DE, O, ST,
MAC/MC), compound (incl. a component within spelling distance), spelling (OSA
distance 1-2, or Jaro-Winkler >= 0.89), different (two surnames each carried by
25+ people nationally), unrelated. Calibration: it calls 7 of 389 matches
corroborated by 2+ shared board members "different" (1.8%; several of those,
e.g. STEWART / SATTERWHITE, look like real different people), and 52% of
different-family surname pairs sharing a phonetic key. Conservative by design.

- 4.2% of cross-org matches at score 7 (3,776) pair two different established
  surnames (LISA PIERCE / ELIZABETH PRICE, DAVID ROSSI / DAVID ROSE): a
  candidate gate.
- 0.6% have two different established first names; 0.6% conflicting middle
  initials; 0.1% conflicting generational suffixes (74).
- Within organizations, 138 records hold two different established surnames
  (JOHN BASON / JOHN GIBSON): within-org merge errors.

**Record checks** (06m), per 10,000 records: 39.8 mix female and male rows; 13.1
hold two established first names; 4.3 differ only by a particle; 0.6 hold two
generational suffixes. No record holds two rows in one filing year (the
within-org linker's year guard prevents it). Cross-org matches on a first name
carried by 1-5 records reach score 10 64% of the time vs 47% for common first
names: first-name rarity is evidence the scorer does not yet weight.

**Surname changes** (06l). Women's count minus what the men's rate predicts:
about 581 changes among 54,485 women's records (10.7 per 1,000), 424 of them
already joined inside one record by the within-org linker. Successions under a
generic board title (BOARD MEMBER MARY -> BOARD MEMBER MARY) are as common
among men as women: coincidence, kept as hard negatives.

**Hard-case library** (`hard-cases/` in the data folder; `_hard_cases.R`,
`hard_cases_read()`): 9,225 cases in 17 categories with labels
expected_same / expected_different / likely_same / likely_different /
uncertain. Ground truth: 581 co-present homonyms, 74 + 8 suffix conflicts,
382 corroborated surname variants. Reviewer labels from the clerical review
join on (emp_a, emp_b).

### Impact on network measures

`name_risk()` (see People above) should return P(same person) per cross-org
person, and the analysis helpers should take a `split = ` argument that splits
persons below a probability. The vignette should show the matched-removal
comparison: the change in each measure from splitting ambiguous persons,
against splitting random persons with the same number of boards.

## Open questions

- Pooled 2010-2024 vs year-by-year networks: offer both (`year =` argument on
  projections) or one default?
- Person threshold: a separate stricter `XORG_ID` (a second `link_cross_org()`
  run at 10), or a per-edge score kept so projections can filter later?
- National associations filing from a local address (AICPA in Durham) form
  their own inner-circle groups; flag them by board geography (most members
  outside the CSA)?

## References

- Krackhardt, D., & Stern, R. N. (1988). Informal networks and organizational
  crises: An experimental simulation. *Social Psychology Quarterly*, 51(2),
  123-140. doi:10.2307/2786835
- netseg: `ei()` and related segregation measures (Bojanowski).
- egor: `EI()`, with the density-rescaled variant.
- Hanneman & Riddle, *Introduction to Social Network Methods*, ch. 8 (group-
  external and group-internal ties).
