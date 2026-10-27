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
| 4 | `04-bucket.R` | the people rows re-laid by EIN bucket for linkage (then re-run step 3) | `people-by-ein/bucket=NN/part-0.parquet`, committed `bucket-summary.csv` |
| 5 | `05-link.R` | within-organization linkage, one EIN bucket a task | `emp-map/bucket=NN`, `link-runs/bucket=NN.rds`, and `profiles/` with `profiles = TRUE` |
| 5b | `05b-profiles.R` | person profiles from an existing `emp-map`, without re-running the linkage | `profiles/bucket=NN`, `profile-runs/bucket=NN.rds` |
| — | `05-status.R` | where an interrupted step 5 got to, and whether any emp-map is truncated | committed `link-status.csv` |
| 6 | `06-score.R` | score the linkage against the `core_dev` baseline | committed `score-<tag>.csv` |
| 7 | `07-orgs.R` | one row per organization, with the group-return flag | `orgs/orgs.parquet`, committed `group-return-summary.csv` |

Cross-organization linkage needs the geography and industry blocking keys, so
`00-cbsa.R` (the ZIP-to-CBSA crosswalk, written to `cbsa/`) runs before step 2.
`dev/partvii-panel/PLAN-cross-org.md` has the order and the open questions.

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
(also on S3). `build-log.csv` (also on S3) has one row each time a step starts
and finishes, giving its build cycle (`build_id`), data version (efile version,
years, efile cache date), its titleclassifier, peopleparser and synthid as
`version@commit`, its run time, and the error message for a failed step. A
step with a "started" row and nothing after it was killed. Build 3
(`20261026-1614`) was backfilled from the step logs. At the start of the build (2026-10-21) every package repo
matched its GitHub head:

| Package | Installed from | Notes |
|---|---|---|
| titleclassifier | GitHub main `1ffb8af` (2026-10-23) | adds PRs #35 (the two batch-dependence fixes below), #36 (crosswalk review, F-030), #37 (CEO and board label set, F-031), #38 (step 09 rules fixed against it, F-032) and #39 (CEO thesaurus by organization type, F-033). The organization type uses the NTEE code and 501(c) subsection, joined from the unified BMF (97.9% of panel orgs) in `01-classify.R`. Build 1 (2026-10-21, `bdfda4e`) is kept locally as `partvii-v3_1-build1/`. |
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

## Layout

| Folder | One file per | Rows | Columns | Use |
|---|---|---|---|---|
| `classified/` | tax year | 60.6M person-title rows | all 153 (source + titleclassifier) | title and role analysis; joins on `OBJECTID` + `TABLE_ID` |
| `people/` | tax year | 58.4M people | 34 | one year at a time |
| `people-by-ein/` | EIN bucket (`bucket=00`-`99`) | 58.4M people, 539k-715k a bucket | 27 linker columns | **linkage**: each bucket holds every year of ~8,000 orgs, so it is a self-contained panel for one worker |

`people-by-ein/`:
- **Bucket** = the last two digits of the EIN. They split organizations
  evenly (7,603-8,591 orgs a bucket). The leading two digits are IRS prefix
  codes and do not (85 groups, 0.0004-4.3% of orgs).
- **Sorted** by `ein`, `taxyr`, `OBJECTID`, `TABLE_ID`, in row groups of
  100,000 rows, so a filter on `ein` reads only the row groups that hold it.
- **Columns:** `PERSON_YEAR_ID`, `OBJECTID`, `TABLE_ID`, `ein`, `taxyr`,
  `formtype`, `org.name`, `org.state`, `org.cbsa` and `org.ntee` (the
  geography and industry blocking passes of cross-org linkage), the name and
  parsed name parts (`name`, `name.raw`,
  `salutation`, `first_name`, `middle_name`, `last_name`, `suffix`, `gender`),
  `title.standard`, `title.raw`, `tot.hours`, `tot.comp`, `trustee.x`,
  `officer.x`, `role.final`, `role.board`, `role.ceo`. Anything else joins
  back from `people/` or `classified/`.

Read timings on this machine (2026-10-22): all 15 years of `people/`, 11
columns, in 48 s from S3. One bucket (about 30 MB) in 1.8 s; one org's
rows in 0.4 s when the filter names its bucket. Within-org linkage
needs one org's records at a time, so it runs one bucket per worker.
Cross-org linkage needs every org, but only the person profiles (at most
about 16M, one per within-org `EMP_ID`) and only profiles that share a
surname blocking key, so it does not need the records at once.

## Reading it

```r
library(arrow)
s3 <- "s3://nccs-efile/dev/synthid/partvii-v3_1"
people  <- open_dataset(file.path(s3, "people"))
# Declare bucket a string: by default arrow reads "07" as the integer 7.
buckets <- open_dataset(file.path(s3, "people-by-ein"),
                        partitioning = hive_partition(bucket = utf8()))

# one bucket = a complete panel for ~8,000 orgs (1.8 s from S3)
b07 <- read_parquet(file.path(s3, "people-by-ein/bucket=07/part-0.parquet"))

# one org, all years: give the bucket (last two EIN digits) so arrow opens one
# file (0.4 s from S3; 14.6 s on ein alone, which opens all 100)
e <- "EIN-02-0513507"
dplyr::collect(dplyr::filter(buckets, bucket == substr(e, 13, 14), ein == e))

# locally:
people <- open_dataset(file.path(Sys.getenv("USERPROFILE"), "Documents/SYNTHID/partvii-v3_1/people"))
```
