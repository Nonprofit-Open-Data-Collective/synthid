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

## Step 4 — build the profiles (`05b-profiles.R`)

```
Rscript dev/partvii-panel/05b-profiles.R <workers> all
```

The reason this exists is cost: profiles took 2-3x the linkage (120-170 min a
bucket), and `05-link.R` builds them only inside `link_bucket()`, skipping any
bucket whose emp-map already exists — so getting profiles for an already-linked
bucket that way means paying for the linkage again.

A profile written before the blocking columns existed is **not** treated as
done: the script checks the parquet schema for `states` / `cbsa` / `ntee` and
rebuilds it if they are missing, since skipping it would silently leave every
pass but `surname` unable to fire. It also asserts that the emp-map covers its
bucket row for row, which fails loudly if the two came from different builds.
`MIN_FREE_GB` is an environment variable (default 12) so the memory gate can be
matched to the machine.

What it does:

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
- Reports, per bucket and weighted overall, the share of profiles that actually
  carry a state / CBSA / NTEE set. A low number there means the matching pass
  will mostly not fire whatever the ladder says, and is the cheap early warning
  before the measurement in step 6.

Run it for every bucket with a valid emp-map; buckets with no emp-map are
reported and left for `05-link.R` (step 5).

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

## Rerun of 2026-10-26 (build 3)

Windows Update restarted the build-2 machine on Fri 2026-10-23 at 23:29.
`05-status.R`: 98 of 100 emp-maps intact, none truncated. Only buckets 14 and 97
were killed. Each is held up by one group return (Hadassah, 39,513 records;
Kappa Kappa Gamma, 17,895), and both had been running ~10 h. Decisions:

- **Re-run step 01** on titleclassifier `91ca48d` (adds F-034, a university
  president outranks a paid chancellor). The change only touches step 09
  roles, not `title.standard` or names, so the 98 emp-maps stay valid; the
  `05b-profiles.R` alignment check confirms that per bucket. Build 2's
  `classified/`, `people/` and `people-by-ein/` are kept in
  `SYNTHID_DIR/partvii-v3_1-build2/`, and `people/` there is the
  `REUSE_NAMES_FROM` input for step 02.
- **Defer buckets 14 and 97** (`deferred-buckets.csv`). `05-link.R ... all`
  skips them; name a bucket to run it anyway. A group return is many chapters
  under one EIN, so within-org linkage is the wrong model for it. Splitting
  these (e.g. by surname key) is a follow-up.
- **Batch cross-org by surname key** (step 8), as above, not by CBSA. A CBSA
  batch holds only the `metro` pass. `strict` and `industry` pairs in one
  state but different (or no) CBSAs, all of `geo` and `surname`, and every
  rural profile (no CBSA) would fall outside any batch. CBSA stays a blocking
  key and an output column, so results can still be summarized by metro.
- **Restart safety.** `05-link.R` and `05b-profiles.R` write each output under
  a temporary name and rename it into place, and log every start / finish /
  failure to `run-ledger.csv`. On restart, buckets killed mid-run are marked
  `interrupted` and re-queued (`_panel.R`).

## Step 6b — test CBSA vs. CSA as the metro blocking unit (after step 4)

Question: is a metro area (CBSA) or a combined commuting region (CSA) the
more defensible geography for the metro pass? A CSA should catch board
interlocks across adjacent metros that cross a state line (e.g. Washington and
Baltimore), which `metro` (different CBSAs) and `strict` / `industry`
(different states) cannot see. The cost is bigger blocks, so more
common-surname blocks go over `max_block_size` and are dropped.

No rebuild is needed: each CBSA belongs to at most one CSA, so the profiles'
`cbsa` sets map to CSA sets at test time (OMB delineation, 2023 vintage, the
same vintage as the CBSA crosswalk).

- **Test region:** Washington-Baltimore-Arlington CSA (DC, MD, VA, WV, PA). It
  has several CBSAs and crosses state lines, so it tests exactly the case CSA
  is meant for.
- **Input:** profiles of every person with a CBSA in that CSA.
- **Arms:** (A) the current ladder; (B) the same ladder plus a
  `region = c("last", "csa", "ntee")` pass between `metro` and `geo`;
  (C) `metro` keyed on CSA instead of CBSA.
- **Compare:** per arm and pass: candidate pairs, dropped blocks, pairs over
  the threshold, and the pairs found only in B or C. Review a sample of those
  with `link_review_queue()` to estimate how many are true matches.
- **Decision:** adopt the CSA pass if the pairs it alone finds hold up in
  review at about the precision of the `metro` pass, and its dropped blocks
  stay few. Record the result as a FINDINGS entry.

## Step 6 results (2026-10-27, build 3)

`06-blocks.R` on the whole panel, not one or two buckets: a block's size depends
on the whole population, and an EIN bucket holds 1% of it. 15.0M persons,
81.6M person-block keys, 7.5M blocks, `max_block_size` 5,000
(`blocks-summary.csv`, `blocks-dropped.csv`, `blocks-surname-cost.csv`).

| Pass | Comparisons | Blocks dropped | People with a usable block |
|---|---|---|---|
| strict | 1.02B | 0 | 95.0% |
| metro | 0.32B | 0 | 77.8% |
| geo | 7.58B | 304 | 82.3% |
| industry | 5.84B | 65 | 97.3% |
| surname | 4.84B | 803 (largest S530, 146,927) | 26.7% |
| all | 19.6B | 1,172 | 99.97% |

- Coverage is not the problem: strict + metro alone reach 95% of persons and
  drop nothing. `metro` drops no block, so `max_block_size` does not need raising.
- The loose passes are 93% of the work. On one surname key (R343, 1.7M pairs),
  the share of scored pairs at or above 7 was strict 2.4%, metro 1.1%, geo 0.26%.
- The surname pass is mostly dropped, as expected.
- Surname-key batches balance well: the costliest key (S530) is 1.2% of all
  comparisons, the top 100 keys 36%.

Two costs for step 8, from the same test:

- **Scoring is slow:** `score_candidate_pairs()` ran at ~1,800 pairs/s per
  worker. At 16 workers that is ~13 h for strict + metro and ~8 days for all
  passes. It compares variant sets pair by pair (`expand.grid` per pair), so
  vectorizing it is the main lever.
- **Profiles must be regrouped by surname key:** finding one key's 1,833
  profiles across the 98 EIN-bucket files took 24 min. The runner's first pass
  should write profiles once into surname-key batches (a profile with k keys
  in each of its batches).

The first `06-blocks.R` run crashed inside `arrow.dll` (0xc0000005) on the
81.6M-row join in arrow's query engine. Part 2 now runs in data.table.

## Step 6b results (2026-10-27): one CSA, Raleigh-Durham-Cary (CSA 450)

`06b-csa-test.R 450`: 118,128 people, 5,692 orgs, 5 CBSAs. The widest
blocking (surname + CSA) was scored once (14.3M pairs, 10 min on 20 workers,
~1,180 pairs/s/worker); the other blockings are subsets of it. Results in
`csa-test-450.csv`; every scored pair in `blocks/csa-test/csa=450/pairs.parquet`.

At threshold 7:

| blocking | pairs | edges | interlockers | org ties | mean degree |
|---|---|---|---|---|---|
| surname + CBSA + NTEE (current metro) | 1.17M | 17,060 | 6,843 | 6,884 | 2.4 |
| surname + CBSA | 7.29M | 66,837 | 16,053 | 41,509 | 14.6 |
| surname + CSA + NTEE | 2.20M | 20,732 | 8,208 | 10,599 | 3.7 |
| surname + CSA | 14.28M | 89,237 | 17,903 | 69,612 | 24.5 |

- **NTEE blocking loses most matches.** 74% of surname + CBSA edges join people
  in different NTEE major groups, and they score like same-NTEE edges (median
  10.1 vs 10.2). Dropping NTEE gives 2.3x the interlockers for 6.2x the pairs.
- **NTEE "UND" (undefined) is the "U" group** seen in step 6: an undefined code
  becomes its own block, so these people only meet each other. About a third of
  sampled cross-NTEE edges have UND on one side.
- **CSA adds less.** +12% interlockers (17,903 vs 16,053) over CBSA for 2x the
  pairs; 25% of CSA edges cross a CBSA line.
- **Org-tie counts are dominated by affiliate families.** The largest clusters
  are the same ~6 board members on 24-46 related housing entities (L20/L21);
  clusters over 10 orgs produce 38% of all org ties. Use interlockers, or ties
  net of these families, as the density measure.
- **Threshold 7 still admits near-miss surnames** (MENDEZ/MENDOZA 8.2,
  PHELPS/PHILLIPS 7.2, BLUE/BLAU 8.1) and common full names (MARIE SMITH,
  JOHN WILSON). Step 7 tuning applies whichever blocking is chosen.
