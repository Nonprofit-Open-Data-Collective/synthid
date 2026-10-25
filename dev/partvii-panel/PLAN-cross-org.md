# Plan: get the panel to cross-organization linkage

Written 2026-10-24, after the blocking work in `9b2c55f`, `103bfde` and
`726c6c8`. The within-organization linkage (`05-link.R`) is built and running;
this is what stands between it and a cross-org run, in the order it has to
happen. Intended to be executed on a faster machine than the one that ran
build 2.

## Why anything has to be redone

Cross-org candidate generation blocks on a surname **and** a geography and an
industry key. `person_blocking_keys()` emits nothing for a pass whose component
the profile lacks, and `build_person_profile()` was called without `state` or
`ntee`, so a profile built before `9b2c55f` can only produce the `surname`
pass. `candidate_pairs()` then drops any bucket over `max_block_size` (5,000
people), which across ~16M profiles is exactly the common surnames — the names
that most need geography to separate them. So the profiles have to be rebuilt
from people rows that carry the new columns.

Two facts make this much cheaper than it looks, and both are worth checking
before trusting the plan:

- **`PERSON_YEAR_ID` is a pure function of `OBJECTID` + `TABLE_ID`**
  (`synthid::person_year_id()`), so rebuilding `people/` and `people-by-ein/`
  does not renumber a single row. An existing `emp-map` still joins.
- **Within-org linkage never reads the new columns.** `synthid_cols()$features`
  is `salutation`, `first_name`, `middle_name`, `last_name`, `suffix`,
  `gender`, `title.standard`; blocking is `pair_blocking(on = ".org")`, and
  `EMP_ANCHOR` is minted from year + `OBJECTID` + `TABLE_ID`. Adding
  `org.state` / `org.ntee` / `org.cbsa` therefore cannot change an `EMP_ID`.

Together: **the within-org linkage does not have to be re-run.** Only the
people tables, the bucket layout, and the profiles do.

## What the new machine needs

`aws s3 sync` in `03-upload.R` copies the whole of `SYNTHID_DIR/partvii-v3_1/`,
so anything that existed locally at the last upload is on S3 — but
`manifest.csv` only lists `people/`, `classified/` and `people-by-ein/`, so
check the prefix itself rather than the manifest.

| Needed by | On S3? | How to get it |
|---|---|---|
| `classified/` | yes | `aws s3 sync` from `s3://nccs-efile/dev/synthid/partvii-v3_1/classified/` |
| `people/`, `people-by-ein/` | yes | same (only needed as `REUSE_NAMES_FROM` input; both get rebuilt) |
| `frame/filings.rds` | **no** | copy from the build-2 machine. Step 02 needs it for the org state |
| `bmf/bmf/*.parquet` | **no** | copy, or re-fetch with `panel990::bmf_retrieve()` |
| `emp-map/`, `link-runs/` | only if an upload ran after the link started | otherwise copy from the build-2 machine, or re-run step 05 |
| `efile/` cache | no | not needed: steps 02-07 never touch it |

Packages: titleclassifier is not needed (step 01 is done), peopleparser and
synthid are. Record versions with `pkg_info()` as the other steps do.

## Step 0 — find out where the interrupted run got to

```
Rscript dev/partvii-panel/05-status.R
```

On the build-2 machine, before copying anything. It reports, per bucket,
whether `emp-map` / `profiles` / the link-run record exist, and checks that
each emp-map file opens and holds as many rows as its `people-by-ein` input —
`05-link.R` decides what to re-run from the presence of that file alone, so one
truncated by the shutdown would be skipped for good. Those print as `BAD` with
the `file.remove()` call that clears them. It writes
`dev/partvii-panel/link-status.csv`, which is in the repo and so syncs back.

Three things to read off it:

1. How many of the 100 buckets have a valid emp-map. Those do not need
   re-linking, on any machine.
2. Whether `profile_min` is `NA` everywhere. If it is, the run was launched
   without the third argument and no profiles exist at all. Either way no
   existing profile is usable (all predate `9b2c55f`), so plan to build them
   all.
3. Any `BAD` buckets. Delete those emp-map files before step 05.

## Step 1 — build the crosswalk (`00-cbsa.R`)

```
HUD_API_TOKEN=... Rscript dev/partvii-panel/00-cbsa.R hud
```

The `metro` pass (`103bfde`) does nothing until the panel carries an
`org.cbsa` column. `00-cbsa.R` builds it: the crosswalk, and the per-filing
`OBJECTID` -> CBSA lookup that `02-people.R` joins. Two decisions it encodes.

**Where the ZIP comes from.** `02-frame.R` already pulls
`F9_00_ORG_ADDR_STATE` from the filing header, so the header almost certainly
carries the ZIP in the same family of fields — check the column names in
`eff_path("F9-P00-T00-HEADER", 2024)`. Preferred: read `OBJECTID` + ZIP from
the header directly in `02-people.R`, keyed on `OBJECTID`, exactly as the state
is read from the frame. The `OBJECTID`s in `classified/` are already the
frame's chosen filings, so no de-duplication is needed and `02-frame.R` does
not have to be re-run. Fall back to the BMF's ZIP only if the header has none —
the BMF is one current address per organization, where the header is per
filing and so follows an organization that moves.

**Where the crosswalk comes from.** Two routes:

- **HUD USPS ZIP crosswalk (preferred, one hop).** `type=3` is `zip-cbsa`, from
  `https://www.huduser.gov/hudapi/public/usps` with a bearer token from a free
  HUD account; `query=all` returns every ZIP. Updated quarterly, and on 2020
  Census geographies from the 2023 Q1 release. The R package `zippeR` wraps it:
  `zi_load_crosswalk(zip_source = "HUD", year = 2023, qtr = 1, target = "CBSA",
  query = "all", key = <token>)`. HUD warns the two directions are not
  inverses, so use `zip-cbsa` (3) and not `cbsa-zip` (8). The file carries
  allocation ratios for ZIPs spanning more than one CBSA — confirm the exact
  column names from the download before relying on them.
- **No-token fallback, two hops.** ZIP (ZCTA) to county from the Census
  relationship files, then county to CBSA from the OMB Bulletin 23-01
  delineation file (NBER mirrors it as a CBSA-to-FIPS-county crosswalk in CSV).
  More joins, no account, and it needs care for Connecticut, which the 2023
  delineation puts on county-equivalent planning regions.

**Three design points that follow from how blocking uses this.**

1. *A straddling ZIP is kept in the crosswalk, but one CBSA reaches the people
   rows.* `build_person_profile()` keeps `cbsa` as a **set** per person, so
   someone filing from two metros gets both keys. But `org.cbsa` is a scalar
   column and the linker asserts one row per person per filing, so a single ZIP
   spanning two CBSAs has to resolve to one — the largest allocation share.
   `zip-cbsa.parquet` keeps every pair above `MIN_SHARE` (0.05), so reaching
   both CBSAs of a straddling ZIP later is a matter of teaching
   `build_person_profile()` to read a delimited or list column; the data is
   already there.
2. *Use one delineation vintage for all 15 years.* The panel spans TY2010-2024
   and the delineations changed in 2013, 2018 and 2023. Blocking needs keys that
   *collide*, not keys that are historically correct for each year: mixing
   vintages would give the same organization different CBSA codes in different
   years and fragment its bucket. Pick the latest and apply it throughout.
3. *A ZIP in no CBSA is fine.* Rural organizations get an empty `cbsa` set, the
   `metro` pass emits nothing for them, and they still block on `strict`,
   `geo`, `industry` and `surname`. No special case needed.

Output: a small parquet of `zip` -> `cbsa` (one row per pair), stored next to
the BMF cache in `SYNTHID_DIR`, plus a note of the vintage and quarter in the
build info.

## Step 2 — re-run `02-people.R`

```
REUSE_NAMES_FROM=<old people/ folder> Rscript dev/partvii-panel/02-people.R
```

Adds `org.state` and `org.ntee` (already committed in `9b2c55f`), and
`org.cbsa` once step 1 lands. `REUSE_NAMES_FROM` skips the name parsing, which
is the slow part — it reuses the earlier build's parsed names and re-parses
50,000 at random to prove they match, stopping if they do not.

**The one unverified assumption in this plan:** that
`classify_titles(preserve_input = TRUE)` kept `ntee_code_clean` in
`classified/`. It was merged into the input before classification in build 2,
so it should be there, but it was never confirmed. If it is missing, the
`col_select = all_of(keep)` read fails immediately and loudly — in which case
join the NTEE code from the BMF in `02-people.R` the same way `01-classify.R`
does, rather than re-running step 01.

Then re-run `03-upload.R`.

## Step 3 — re-run `04-bucket.R`

```
Rscript dev/partvii-panel/04-bucket.R
Rscript dev/partvii-panel/03-upload.R
```

Picks up the new columns via `LINKER_COLS` (26 columns as of `9b2c55f`, plus
`org.cbsa`). Note it opens with `unlink(pv_bkt, recursive = TRUE)`: the bucket
layout is rebuilt whole, not incrementally, so let it finish. Row counts and
`PERSON_YEAR_ID`s must match `people/` exactly — the same check the original
build made.

## Step 4 — add a profiles-only step, and run it

New script, `05b-profiles.R`. This is the piece of work that does not exist
yet, and the reason it matters is cost: profiles took 2-3x the linkage
(120-170 min a bucket), and `05-link.R` builds them only inside
`link_bucket()`, skipping any bucket whose emp-map already exists. As the code
stands, getting profiles for an already-linked bucket means re-running the
linkage for it.

Spec:

- Input, per bucket: `emp-map/bucket=NN/part-0.parquet` and
  `people-by-ein/bucket=NN/part-0.parquet`.
- Join on `PERSON_YEAR_ID` (stable, see above). The emp-map supplies `EMP_ID`
  and `EMP_ANCHOR`; the bucket supplies the parsed name parts,
  `title.standard`, `ein`, `taxyr`, `org.state`, `org.ntee`, `org.cbsa`. That
  is everything `build_person_profile()` reads.
- Call `build_person_profile(df, state = "org.state", cbsa = "org.cbsa",
  ntee = "org.ntee")` and write `profiles/bucket=NN/part-0.parquet`.
- Assert the row count equals the bucket's `uniqueN(EMP_ID)`, and that
  `EMP_ANCHOR` came through.
- Same shape as `05-link.R`: one bucket per `callr` process, a free-memory
  gate, costliest first, a bucket already written is skipped, progress
  appended to a log.

Run it for every bucket with a valid emp-map. Buckets with no emp-map go
through `05-link.R` first (step 5).

## Step 5 — finish the within-org linkage

```
Rscript dev/partvii-panel/05-link.R <workers> all FALSE
```

Only for the buckets step 0 showed as missing or `BAD`. `profiles = FALSE`
now, because step 4 builds them more cheaply. On a machine with more cores,
raise the worker count but keep the `MIN_FREE_GB` gate — the per-bucket memory
is what forced one process per bucket in the first place.

## Step 6 — measure the candidate set before running cross-org

On one or two buckets, before committing to a full run. The three changes of
24 October all widen candidate generation, and none has been measured on real
data:

- `strict` / `geo` / `industry` can now fire at all (`9b2c55f`).
- `metro` is new (`103bfde`).
- Compound surnames now emit a key per component (`726c6c8`), and those
  components are often commoner surnames than the compound itself, so some
  buckets grow.

Report, per pass: buckets, distinct people, `attr(cand, "n_comparisons")`, and
`attr(cand, "dropped_blocks")` — how many buckets exceeded `max_block_size`
and which passes they came from. If the surname pass is dropping nearly
everything, that is expected and is what the tighter passes exist to fix; if
`metro` or `geo` buckets are being dropped, `max_block_size` needs raising or
the CBSA allocation threshold tightening.

## Step 7 — tune the cross-org threshold

`link_cross_org()` defaults to `threshold = 7`, inherited from within-org, and
its own documentation flags that it **needs separate tuning** because the score
scale differs — within-org scores two records, cross-org scores two profiles
built from variant sets. `dev/tune_cross_org_threshold.R` exists; point it at a
bucket's profiles and pick a threshold before any full run.

## Step 8 — the cross-org runner

Not written. Design, from the 24 October discussion:

- **Batch by surname blocking key, not by EIN.** Every pass in
  `blocking_passes()` includes `last`, so two profiles can only ever become a
  candidate pair if they share a surname key. Partitioning profiles by surname
  key is therefore a complete partition of the candidate space — no pair can
  span two batches, and the result is identical to one big run. EIN buckets are
  the right unit for within-org and the wrong one here.
- A profile with *k* surname keys must be replicated into all *k* batches, and
  pairs found in more than one batch deduplicated (`n_shared_buckets` has to be
  summed across batches, not taken from one).
- `max_block_size` is per bucket, so batching does not change which buckets are
  dropped.
- Soundex is heavily skewed — `S530` (SMITH and friends) will dwarf the rest —
  so order batches by estimated cost and load-balance, as `05-link.R` does for
  EIN buckets.

## Not in this plan

- The 650 crosswalk proposals in `dev/CROSSWALK-UPDATES.md`, still unreviewed
  and explicitly excluded from build 2.
- Re-running step 01. Nothing here needs the titles reclassified.
- The `README.md` plan table, which still stops at step 4 and does not mention
  `05-link.R`, `05-status.R`, `06-score.R` or `07-orgs.R`.

## Sources for step 1

- HUD USPS ZIP crosswalk API: <https://huduser.gov/portal/dataset/uspszip-api.html>,
  <https://huduser.gov/apps/public/uspscrosswalk/home>
- `zippeR::zi_load_crosswalk()`: <https://rdrr.io/cran/zippeR/man/zi_load_crosswalk.html>
- County-to-CBSA (OMB Bulletin 23-01, NBER mirror):
  <https://www.nber.org/research/data/census-core-based-statistical-area-cbsa-federal-information-processing-series-fips-county-crosswalk>
- 2023 delineation background: <https://www.bls.gov/cew/classifications/areas/county-msa-csa-crosswalk.htm>
