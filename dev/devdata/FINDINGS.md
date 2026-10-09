# Development dataset: decisions and findings log

Decisions (D-nnn) record a design choice and its reason. Findings (F-nnn)
record what a build or check turned up, with evidence and status: **open**,
**fixed** (with the commit), or **noted**. The process is in
[README.md](README.md).

## Decisions

### D-001 Window TY2010-2024, membership by panel990's classes

v3.1 carries TY2009-2025, but 2009 and 2025 are partial years: the 2025
header file is 22 MB against 63 MB for 2024. The window is the 15 complete
years, the same span as `dev/test_panels_1k`.

Each organization gets panel990's two classes, computed over the window from
the years it has a filing:
- `panel_type`: persistent / entrant / exit / transient;
- `panel_spell`: seamless / segmented.

The unit is one filing per organization-year, the latest submission, so an
amended return replaces the original. This is the rule titleclassifier's
calibration sample used.

`panel990::panel_describe()` builds one data frame per organization and binds
them, which doesn't scale to the full frame. `02-frame.R` applies the same
rules vectorized and asserts 100% agreement with `panel_describe()` on a
random 20,000 organizations.

### D-002 Cohorts

| Cohort | Rule | Orgs | Purpose |
|---|---|---|---|
| `core_dev` | persistent + seamless | 3,000 | the stable long panel to tune on |
| `core_holdout` | persistent + seamless | 3,000 | the same population, held out for checking |
| `entrant` | first filing after 2010, files through 2024, no gaps | 1,500 | people arriving with a new filer; left-censored careers |
| `exit` | files from 2010, last filing before 2024, no gaps | 1,500 | right-censored careers; final returns |
| `gapped` | one or more interior missing years (any `panel_type`) | 1,500 | linking across gaps; the long-absence and pay-break anomaly flags |
| `transient` | enters after 2010 and leaves before 2024, no gaps | 1,000 | short panels at both edges |

- The two core cohorts are one stratified draw, split in half within each
  stratum, so they are comparable.
- Strata: organization size (median expenses over its filings: $0, $1-150k,
  $150k-1m, $1m-10m, $10m-100m, $100m+, missing) x form mix (990 only,
  990-EZ only, switcher).
- Allocation is proportional to the square root of the stratum population,
  with a floor of 25 per stratum. This over-represents large and rare
  strata, and `weight` restores population shares.

### D-003 Leave out the organizations that trained titleclassifier's role model

Step 09's 990-EZ role model (titleclassifier F-028/F-029) was trained on:
- every full-990 person in titleclassifier's 2019-2023 calibration sample
  (35,396 organizations);
- the 990-EZ people of about 4,400 form-switching organizations.

Role accuracy measured on organizations the model trained on would be
inflated, so they are excluded from the draw. Neither EIN list is in the
titleclassifier repository, so `00-tc-training-eins.R` rebuilds both. It runs
titleclassifier's own sampling script on the v2.3 tables and requires its
cell-count file to match the committed one byte for byte.

Overlap with the older `dev/test_panels_1k` EINs is flagged
(`in_test_panels_1k`), not excluded, so results can be compared on shared
organizations.

### D-004 What is pulled

| Table | Why |
|---|---|
| Header | form, tax period, amended / group / initial / final flags, principal officer name |
| Part I (990, EZ) | expenses (size), employees, voting members |
| Part VI (990, EZ) | voting and independent members, governance policies |
| Part VII totals (DIR-TRUST-KEY) | counts of people over $100k, total pay |
| Part VII Section A | the people |
| Schedule J Part II | itemized pay for the highest-paid people |
| BMF (`panel990::bmf_retrieve`) | NTEE, subsection, ruling year, state, geography |

Only the chosen filing per organization-year is kept. Superseded submissions
stay counted in `filings$n_submissions`.

### D-005 Entry and exit are not births and deaths

Added after F-002. Two adjustments:
- Exit and transient organizations whose last filing is TY2023 are dropped.
- Entrants, exits and transients are stratified on the boundary filing's
  initial-return / final-return flag (`boundary` column).

The flagged true births and deaths (`initial`, `final`) are then a sizable
part of each cohort, not a few percent.

| Cohort | Boundary | Orgs |
|---|---|---|
| entrant | initial return | 620 |
| entrant | no flag | 1,047 |
| exit | final return | 665 |
| exit | no flag | 934 |
| transient | initial and final | 305 |
| transient | one flag or none | 1,289 |

## Findings

### F-001 The frame (noted)

Built by `02-frame.R`:
- 6,052,454 submissions, TY2010-2024, become 5,964,599 org-year filings
  (latest submission) from 791,019 organizations.
- 1.4% of org-years have more than one submission. 0.13% are group returns.
- Every filing, 990 and 990-EZ, has Part VII rows: 990-EZ Part IV is in the
  same table.
- The vectorized membership classes agree with `panel990::panel_describe()`
  on 20,000 random organizations (100%).

| panel_type | panel_spell | Orgs | Median years |
|---|---|---|---|
| persistent | seamless | 88,546 | 15 |
| persistent | segmented | 39,481 | 13 |
| entrant | seamless | 360,232 | 6 |
| entrant | segmented | 73,161 | 9 |
| exit | seamless | 44,680 | 5 |
| exit | segmented | 12,469 | 7 |
| transient | seamless | 149,557 | 2 |
| transient | segmented | 22,893 | 5 |

Full table: `frame-summary.csv`.

All 39,896 organizations that trained titleclassifier's role model are in
the frame and excluded (D-003). `00-tc-training-eins.R` reproduced the
calibration sample exactly: titleclassifier's `panel-sample-cells.csv` came
out byte-identical, with 60,520 filings and 35,396 organizations as in
titleclassifier F-025, plus 4,500 switcher organizations.

### F-002 Most entrants are new to e-filing, not new organizations (noted, shaped D-005)

Filings per year roughly triple, from 185,176 in TY2010 to 561,420 in
TY2024. Mandatory e-filing came in for TY2020+ returns. So an organization's
first e-filed return is usually not its first return.

| First year | Entrants | First filing is an initial return |
|---|---|---|
| 2011 | 77,258 | 4% |
| 2015 | 37,491 | 16% |
| 2020 | 58,406 | 18% |
| 2021 | 57,184 | 24% |
| 2022-2024 | ~34,000 a year | 39-40% |

Exits are the same: only 12-21% of last filings are flagged final returns.
The rest stop e-filing, for example by moving to the 990-N, losing exemption
or filing late.

**Late filers:** 39,883 organizations have a last filing in TY2023, against
22,774-26,900 for TY2021-2022. Many are presumably TY2024 returns not yet in
v3.1, since 2025 is still filling in. Exits last seen in 2023 are excluded
(D-005).

**For synthid:** a person "leaving" at an organization's last e-filed year
is often censoring, not a departure. Transition and churn statistics near the
panel edges should condition on the `boundary` flag.

### F-003 The development dataset (built)

`04-extract.R` with seed 2026 writes `SYNTHID_DIR/devdata/`:

| Table | Rows | Columns |
|---|---|---|
| orgs | 12,355 | 58 |
| filings | 128,964 | 209 |
| persons | 1,605,317 | 44 |
| schedj | 199,194 | 36 |

| Cohort | Orgs | Filings | Part VII rows |
|---|---|---|---|
| core_dev | 2,990 | 44,850 | 626,974 |
| core_holdout | 2,982 | 44,730 | 598,942 |
| entrant | 1,667 | 11,015 | 100,317 |
| exit | 1,599 | 8,968 | 86,205 |
| gapped | 1,523 | 13,826 | 153,698 |
| transient | 1,594 | 5,575 | 39,181 |

- 359 core organizations are also in `dev/test_panels_1k`.
- BMF fields match 97.8% of organizations. Transients are lowest at 90.7%,
  since dead organizations drop out of the BMF.
- About 100 990-EZ filings a year (1% of EZ filings) have no Part I summary
  row, so `size_year` is `missing`.

### F-004 The independent checks are usable on full 990s only (noted)

From `06-profile.R`, written to `profile-filings.csv`:

| Check | Full 990 | 990-EZ |
|---|---|---|
| Principal officer named in the header | 83-88% of filings | none |
| Voting members (Part I line 3 = Part VI line 1a) | 100% (the two always agree) | none |
| Schedule J filed | 25-38% | none |
| Part VII "people" that are organizations | 2-4% of rows | under 0.6% |

- **Trustee box count vs. voting members:** they are equal in only 50-56% of
  990 filings and within 2 in 72-79%. The voting-member count is a separate
  board-size check, not a copy of the trustee boxes.
- **990-EZ has none of these checks.** Validation for 990-EZ people has to
  come from form switchers (titleclassifier F-029) and from cross-year
  consistency.

### F-005 Parse distinct names only (fixed in 05-classify.R)

`peopleparser::parse_names()` is a pure per-string map:
- `sanitize_input()` and `parse_name()` each see one name at a time;
- nothing depends on the other names in the batch.

So parsing the distinct strings and joining back by `match()` gives exactly
the same result.

| Scope | Name rows | Distinct names | Rows per name |
|---|---|---|---|
| Dev dataset (primary rows) | 1,605,317 | 430,707 | 3.7 |
| All Part VII, TY2010-2024 | 59,292,642 | 9,255,729 (upper-cased, trimmed) | 6.4 |

The ratio grows with scale. Each added year brings about 5M rows but only
about 550k new names.

**Further, not done here:**
- A persistent parse cache keyed by the raw string, so each wave parses
  only new names (the same add-a-wave pattern as `link_incremental()`).
- Batch the work inside `parse_names()`. It sends each name to a worker as
  its own task and binds 1-row data frames, and that overhead probably costs
  more than the parsing itself.

**Measured on this build:** parsing all 1,605,317 rows took 37.1 min, and
parsing the 430,707 distinct names took 9.0 min (4.1x faster).

### F-006 Step 09 roles on the development dataset (noted)

`05-classify.R` with titleclassifier `8de324d`:
- 1,605,317 Part VII people become 1,690,065 person-title rows.
- Every person has exactly one `role.primary` row.
- The rules decide 1,366,362 people; the 990-EZ model decides 238,955.

| Role | Full 990 | 990-EZ |
|---|---|---|
| BOARD | 81.3% | 94.7% |
| OFFICER | 7.2% | 1.7% |
| CEO | 4.8% | 3.5% |
| STAFF | 3.9% | 0.1% |
| PROFESSIONAL | 1.6% | 0.0% |
| MANAGER | 1.3% | 0.1% |

`people.parquet` is synthid's input: it is in `synthid_cols()` layout plus
step 09's role columns.

### F-007 Baseline linkage on core_dev (noted)

`07-link.R` ran `link_panel()` with default settings (threshold 7, weighted)
on core_dev. Setup: synthid `225ba86`, 20 workers, organizations balanced on
records². `08-score.R` scored the result, and all metrics are in
`baseline-core_dev.csv`.

**Run:**
- 626,974 records in 2,990 organizations became 132,994 persons (78.8%
  compression).
- 18.5 min wall time, 168 min of worker time. The largest organization
  (4,784 records) sets the wall time.
- 80.0% of persons appear in 2+ years; the median is 4 years.
- No person has two records in one year.

**Linkage, checked against the principal officer** (full 990s, 19,045 and
3,048 consecutive-year pairs):

| Check | Result |
|---|---|
| Same principal officer (same parsed first and last name) next year: same EMP_ID | **99.85%** |
| Clearly different principal officer next year: same EMP_ID | **0.00%** |
| The step 09 CEO is the same EMP_ID next year | 90.6% (titleclassifier F-026: 88-92%) |

Within-organization linkage holds up on the people it can be checked
against. Principal officers are stable, well-recorded executives, though, so
this is an easy test. Board members and homonyms are where to look next (see
the flags).

**The principal officer is a noisy check on the CEO, not ground truth:**
- It appears in Part VII on 87.5% of filings that name one.
- Step 09 calls it CEO 44% of the time and BOARD 48% of the time.

| Leadership type (step 09) | Principal officer is the CEO |
|---|---|
| designated | 70.7% (12,643 filings) |
| imputed | 53.0% (4,845) |
| board_governed | 0% by definition (1,300) |
| no_paid | 0% by definition (7,393) |

- When the principal officer is labeled BOARD, the median is 2 hours a week,
  15% are paid and 87% tick the officer box.
- They are mostly BOARD PRESIDENT (7,921) or BOARD TREASURER (2,346).
- Many filers name the board chair or treasurer as principal officer even
  when there is an executive director: 33% of these filings do have a
  different CEO.
- **Leads for step 09 review:** principal officers that step 09 labels BOARD
  while titled EXECUTIVE DIRECTOR (71), ADMINISTRATOR (28), CHIEF FINANCIAL
  OFFICER (23), CEO (10) or MANAGING DIRECTOR (10). These are probably unpaid
  executives, which is guide rule 7 in titleclassifier F-024.

**Board size vs Part VI voting members** (34,232 990 filings):

| Count | Exact | Within 2 |
|---|---|---|
| Step 09 board seats | 53.7% | **85.8%** |
| Part VII trustee boxes | 54.7% | 76.2% |

Step 09 is closer within 2 seats, and both have a median difference of 0.
`role.board` is set only on BOARD rows, so a paid CEO who also sits on the
board is not counted as a seat. That is worth checking as a source of the
remaining gap.

**Review flags** (persons out of 132,994):

| Flag | Persons |
|---|---|
| `flag_links` nickname_or_spelling | 8,208 |
| `flag_links` surname_compound | 3,526 |
| `flag_links` review | 2,712 |
| `flag_links` order_swap | 1,263 |
| `flag_anomalies` sequential_homonym_gap | 2,543 |
| `flag_anomalies` homonym_copresent | 610 |
| `flag_anomalies` intermittent | 404 |
| `flag_anomalies` sequential_homonym_pay | 123 |
| `flag_anomalies` entity_not_person | 115 |
