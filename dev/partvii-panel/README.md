# Part VII panel, efile v3.1 (full population)

A clean build of every Part VII Section A row in the efile v3.1 release,
TY2010-2024, run through titleclassifier (steps 01-09) and peopleparser. It
gives synthid's linker its input for the whole population, not only the
development cohorts in `dev/devdata/`.

- **Data:** `SYNTHID_DIR/partvii-v3_1/` (default `~/Documents/SYNTHID`), not in the repository.
- **Published copy:** `s3://nccs-efile/dev/synthid/partvii-v3_1/`
- **Scripts:** here. Run from the package root, in order. Each step can be re-run.

## Plan

| Step | Script | Does | Output (`partvii-v3_1/...`) |
|---|---|---|---|
| 0 | `dev/devdata/01-download.R`, `02-frame.R` | (already built) efile v3.1 cache and the frame of one filing per org-year | `efile/`, `frame/filings.rds` |
| 1 | `01-classify.R` | titleclassifier steps 01-09 on each tax year | `classified/classified-<year>.parquet` |
| 2 | `02-people.R` | one row per person per filing (the `role.primary` title row), names parsed once per distinct string | `people/people-<year>.parquet`, committed `panel-summary.csv` |
| 3 | `03-upload.R` | manifest (rows, bytes, md5) and `aws s3 sync` | the S3 prefix above |

Rules carried over from the development dataset:
- **Filing unit:** the latest submission per EIN x tax year (`02-frame.R`), so
  an amended return replaces the original. Rows from superseded filings are dropped.
- **Years:** TY2010-2024. 2009 and 2025 are partial in v3.1.
- **People layout:** the same columns as `dev/devdata` `people.parquet`, without `cohort`.

## Scale

| | |
|---|---|
| Filings (frame) | 5,964,599 |
| Part VII rows kept | about 58.4 million |
| Rows per year | 2.06M (2010) to 5.07M (2024) |

## Package versions

Recorded at run time in `build-info-classify.csv` and `build-info-people.csv`
(also on S3). At the start of the build (2026-10-21) every package repo
matched its GitHub head:

| Package | Installed from | Notes |
|---|---|---|
| titleclassifier | GitHub `bdfda4e` (branch `fix/comma-helper-null`, [PR #35](https://github.com/Nonprofit-Open-Data-Collective/titleclassifier/pull/35)) | main `104c92a` (PR #34: board-title crosswalk patch, taxonomy orphans pruned) plus the two batch-dependence fixes below |
| peopleparser | local `cb01547` (master) | |
| panel990 | `7cd26a6` | head `3a5188a` changes docs only; not used here (efile cache already built) |
| synthid | local `9b2cbd8` | only `person_year_id()` is used |

## Build notes (2026-10-21)

- **Parallelism.** `classify_titles(workers = N)` serializes its whole input to
  every worker (its furrr lambda captures the calling environment): a full
  year put about 6 GB in each of 22 workers and ran at about one core's speed.
  `01-classify.R` instead sends fixed chunks of 500 filings to a PSOCK
  cluster and runs `classify_titles(workers = 1)` in each worker. TY2010
  (2.06M rows) took 7.3 min on 16 workers.
- **Batch dependence (titleclassifier, fixed in PR #35).** Two bugs made
  results depend on which filings shared a batch. With both fixed, 2,000
  TY2010 filings give identical output (every column, including
  `person.id`) under two different batchings.
- *Bug 1:* `comma_helper()`
  (`R/03-standardize-conj.R`) returns `NULL` for a comma title that is neither
  a separator nor an "of" comma. `standardize_conj()` calls
  `unlist(lapply(TitleTxt, comma_helper))`, which drops the `NULL`s, and
  `ifelse()` then recycles the shorter vector. Every comma title after the
  first `NULL` in a batch can get another title's comma decision, so results
  depend on which filings share a batch. In TY2016, 1.0% of rows have a comma
  and 6,570 rows (4,263 distinct titles) hit the `NULL` case. With
  different batching, 2,000 TY2010 filings differed on 1 split and 3-8 title
  fields. Fix: that case is now type 1 (comma becomes a space), and
  `vapply()` replaces `unlist(lapply())`.
- *Bug 2:* `get_row_id()` hashed `.tc_row_id` (a position in the input) and
  went through `as.matrix()`, so `person.id` changed with batching, and with
  it the step 09 tie-break between imputed CEOs with equal pay (3 of 2,000
  filings). Fix: the hash leaves out `.tc_row_id` and pastes each column with
  `as.character()`. Ids for all-character input are unchanged, and
  `person.id` is now a stable key.

Not in this build: the 650 crosswalk proposals in `dev/CROSSWALK-UPDATES.md`
(not yet reviewed or applied to titleclassifier).

## Reading it

```r
library(arrow)
people <- open_dataset("s3://nccs-efile/dev/synthid/partvii-v3_1/people")
# or locally:
people <- open_dataset(file.path(Sys.getenv("USERPROFILE"), "Documents/SYNTHID/partvii-v3_1/people"))
```
