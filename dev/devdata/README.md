# synthid development dataset (efile v3.1)

A fixed, versioned test environment for refining synthid. It's a long
TY2010-2024 panel of organizations that file every year, plus comparison
cohorts that enter late, leave early, skip years or appear only in the
middle. Each organization comes with its Part VII people, the filing-level
tables that give independent checks on those people, and BMF traits.

- **Data:** `SYNTHID_DIR` (default `~/Documents/SYNTHID`), not in the repository.
- **Scripts, process notes and findings:** here, in `dev/devdata/`.
  - [FINDINGS.md](FINDINGS.md) logs decisions (D-nnn) and findings (F-nnn).

## Why a development dataset

The earlier fixtures were built from different sources:
- `dev/test_panels_1k`: v2.x Part VII CSVs, persistent orgs only, no filing tables;
- `dev/data/slice_2010_2012_1000eins`: three years only.

They can't test entry, exit or gaps, and they hold nothing to validate people
against. The efile v3.1 release (2026-10-09) is a clean point to rebuild from,
and titleclassifier now ships step 09 roles that the linker should use. Every
later tuning or regression run should name the dataset build it used.

## Build

Run from the package root, in order. Each script can be re-run, and a step
whose output exists is reused where noted.

| Script | Does | Output (`SYNTHID_DIR/...`) |
|---|---|---|
| `00-tc-training-eins.R` | Rebuilds the list of orgs that trained titleclassifier's role model, checked byte for byte against titleclassifier's committed cell counts | `frame/tc-training-eins.rds` |
| `01-download.R` | `panel990::download_tables()` for the 8 tables x TY2010-2024 | `efile/<year>/*.parquet`, `efile/download-manifest.rds` |
| `02-frame.R` | One filing per org-year (latest submission); per-org membership class (panel990 rules, vectorized and checked against `panel_describe()`) | `frame/filings.rds`, `frame/orgs.rds` |
| `03-sample.R` | Stratified draw of the cohorts | `frame/sample-orgs.rds` |
| `04-extract.R` | Pulls every table for the sampled orgs; BMF via `panel990::bmf_retrieve()` | `devdata/{orgs,filings,persons,schedj}.parquet` |
| `05-classify.R` | titleclassifier steps 01-09 (with the 990-EZ role model) and peopleparser names | `devdata/classified/<year>.parquet`, `devdata/people.parquet` |
| `06-profile.R` | Coverage of each table and of the independent checks, by cohort and form | committed `profile-*.csv` |
| `07-link.R` | `link_panel()` on one cohort, parallel over orgs balanced on records² | `linked/<cohort>.parquet`, `linked/<cohort>-run.rds` |
| `08-score.R` | Scores a linked cohort: principal-officer recall and false merges, CEO agreement, board size vs voting members, review flags | committed `baseline-<cohort>.csv` |

Committed summaries: `frame-summary.csv`, `sample-cells.csv` and `profile-*.csv`.

`people.parquet` is the linker's input. It has one row per listed person per
filing, in `synthid_cols()` layout (`ein`, `taxyr`, `name`, the parsed name
parts, `title.standard`), plus `cohort`, `formtype`, pay, hours, the Part VII
boxes and step 09's `role.*` columns. To link one cohort:

```r
library(arrow); library(synthid)
people <- read_parquet(file.path(Sys.getenv("USERPROFILE"), "Documents/SYNTHID/devdata/people.parquet"))
linked <- link_panel(subset(people, cohort == "core_dev"))
```

## Package versions

| Package | Version |
|---|---|
| panel990 | `7cd26a6` (main, efile v3.1 default) |
| titleclassifier | `8de324d` (main, step 09 with the role model, F-029) |
| efile release | `v3_1`, published 2026-10-09 |

## The dataset

| Table | Grain | Key | Main contents |
|---|---|---|---|
| `orgs` | organization | `EIN2` | cohort, stratum, weight, membership class, form mix, overlap flags, BMF (NTEE, subsection, ruling year, state, geo) |
| `filings` | org-year | `OBJECTID` (also `EIN2` x `TAX_YEAR`) | header (form, tax period, amended / initial / final, **principal officer name**), Part I (expenses, employees, **voting members**), Part VI (**voting members**, independent members), Part VII totals |
| `persons` | person-row | `OBJECTID` x `TABLE_ID` | every Part VII Section A column: name, title, hours, boxes, pay |
| `schedj` | person-row | `OBJECTID` x `TABLE_ID` | Schedule J Part II pay breakdown (base, bonus, deferred, ...) |

### Independent checks the filing tables provide

- **Principal officer** (header, `F9_00_PRIN_OFF_NAME_PERS`): the person the
  filer names as principal officer, on about 82% of 990s (not on the 990-EZ).
  It's a CEO check that doesn't depend on Part VII titles or boxes.
- **Voting members of the governing body** (Part I line 3, Part VI line 1a):
  the board size. Compare with the people step 09 calls BOARD.
- **Initial / final return flags** (header): tell a real birth or death apart
  from a gap in the e-file data at a cohort boundary.
- **Schedule J:** a second, itemized report of pay for the highest-paid people,
  matched by name. It checks Part VII pay and person matching.
