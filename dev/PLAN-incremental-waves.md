# Dev plan — incremental waves & backfill (`link_incremental`)

Objective: support **adding a new wave of data** (or backfilling missing prior
years) to an already-linked panel **without re-issuing existing person IDs**.
Today the only entry points are the two symmetric batch builds — `link_panel`
(within-org, all record-pairs) and `link_cross_org` (profile-vs-profile). A wave
is a genuinely different, third mode, and the current ID recipe makes the naive
"just re-run on old + new" approach re-mint IDs for everyone who gains a record.

Decision taken (2026-08-14): **anchor-to-first-record IDs**, design-only pass.

---

## 1. Why a wave is a third mode

| Mode | Compares | Symmetry | ID minting |
|---|---|---|---|
| Initial build (`link_panel`) | all record-pairs within an org × year-pairs | symmetric batch | hash(sorted member row-keys) |
| Cross-org (`link_cross_org`) | person profiles ↔ profiles, hash-blocked | symmetric batch | hash(sorted member EMP_IDs) |
| **Add-a-wave (this plan)** | **new records → existing frozen profiles** | **asymmetric, append-only** | **inherit or mint (anchored)** |

The *matching mechanic* of a wave resembles cross-org — block the new records
against an existing set, score profile-vs-record from the variant sets, don't do
all-pairs. But the *operation* is asymmetric: the existing person set is frozen,
and each new record must either **attach** to an existing person or **spawn** a
new one. That append requirement is what neither existing mode has.

## 2. The load-bearing problem: IDs are not append-stable

`assign_emp_ids()` (`R/cluster.R:85`) mints:

```r
canon <- paste(sort(cluster_row_uids), collapse = "||")
EMP_ID <- create_emp_ids(canon)          # hash of the FULL membership
```

So "stable" today means *reproducible across re-runs of the same data*, **not**
stable as the panel grows. Add a 2024 row to Jane Roe's cluster → her member set
changes → the canonical string changes → **her EMP_ID changes**. `XORG_ID` (hash
of sorted member EMP_IDs) inherits the identical property. Any downstream table
keyed on EMP_ID would silently break on every wave.

## 3. ID scheme: anchor to first record

Replace hash-of-membership with **hash-of-anchor**:

```
EMP_ANCHOR  = the person's anchor record key (a stable native key, see below)
EMP_ID      = anchor_emp_id(EMP_ANCHOR)      # hash of the anchor alone
```

Because the ID depends only on the anchor — not on the rest of the membership —
adding records to a person never changes their ID.

### 3.1 What the anchor key is
Use the most durable native key available, in preference order:

1. **`PERSON_YEAR_ID` (PYID)** = hash(OBJECTID, TABLE_ID) — already implemented
   (`R/id.R:85`, `person_year_id()`). Parser-independent OBJECTID + stored
   TABLE_ID; unaffected by name/title cleaning.
2. Fallback where PYID is absent: the current `.row_uid` id-string
   (`build_id_string()`), with the documented caveat that it shifts if name/title
   normalization changes.

### 3.2 Anchor selection — "first seen", not "earliest in time"
The anchor must be computable from the cluster **and must not move when other
records are added — including *earlier* records** (backfill is in scope). So the
anchor is **frozen at first creation and carried on the panel**, not recomputed
as the min year each run:

- **Initial build:** anchor = the cluster's deterministic-earliest record —
  `min(tax year)`, ties broken by `min(OBJECTID)` then `min(TABLE_ID)`. Fully
  deterministic, independent of row order.
- **Incremental:** an existing person **keeps the `EMP_ANCHOR` carried in the
  input panel**, whether the new record is later (forward wave) or earlier
  (backfill). Only brand-new persons get a fresh deterministic-earliest anchor
  from their own new records.

This makes IDs stable under **both** append and backfill. The cost the decision
accepts: a **one-time re-mint** of existing IDs when switching schemes (old
membership-hash IDs → new anchored IDs), plus carrying one extra column.

> Change `assign_emp_ids()` so the *initial build* uses anchored IDs too, so
> `link_panel` and `link_incremental` share one recipe. Bump a `keyspec`-style
> version tag on the ID so pre/post-migration IDs can never silently collide.

## 4. The `link_incremental()` pipeline

Inputs: `existing` (a linked panel carrying `EMP_ID`, `EMP_ANCHOR`, name
components, org, year) and `new` (parsed wave, same schema, no IDs yet).

1. **Profile the existing panel** → `build_person_profile(existing)` (extended to
   pass `EMP_ANCHOR` through). One profile per existing person: variant sets,
   orgs, years, anchor. Profiles are cacheable between waves (parquet/rds) so a
   wave need not re-profile all history — dovetails with the DuckDB batching
   workflow.
2. **Block new records → existing profiles, within org.** Candidate = (new row
   `r`, profile `p`) where `p.orgs ∋ r.org`. Optionally add a surname-key bucket
   for recall/speed, mirroring the cross-org hash-block (`person_blocking_keys` /
   `candidate_pairs`).
3. **Invariant guard.** A new (org, year) row may only match a profile that does
   **not already hold a record for that org-year** — same "one record per
   org-year = one person" rule enforced in `resolve_clusters` / `resolve_cross_org`.
4. **Score** each (new row, profile) candidate by reusing the cross-org
   set-scorer: `.best_set_sim()` over the profile's variant sets with
   `compare_last_names` / `compare_first_names`, `score_pairs()`. Surname
   frequency uses the **within-org** `surname_weight()` (family-board logic), not
   the population weight — a wave attaches within an org.
5. **Assign** each new row to its best profile above `threshold`, **greedy
   one-to-one within (org, year)** via `greedy_one_to_one()`: a person absorbs ≤1
   new row per org-year; a new row attaches to ≤1 person. Matched rows **inherit**
   that person's `EMP_ID` / `EMP_ANCHOR`.
6. **Residual new rows** (matched nothing): run the *initial-build* linker
   (`link_panel`) on the residual subset alone to cluster first-time persons who
   appear in ≥2 new rows (new person across two orgs, or a multi-year wave), then
   mint fresh anchored IDs.
7. **(Follow-on)** re-run `link_cross_org` for affected persons — new interlocks
   can appear. Out of scope for v1; note it.

Output: `list(new_stamped, report, id_crosswalk, review)` — new rows stamped with
EMP_ID/EMP_ANCHOR; counts (matched-to-existing / new-persons / invariant-rejected);
the crosswalk (§5); and an ambiguous-match review queue.

## 5. Merges & splits — never silent

Incremental ER can surface that two existing persons are actually one (or that
one ID should split). Append-only wave matching avoids most of this because it
never re-compares existing-vs-existing — but two cases remain:

- A new row scores well against **two** existing profiles (ambiguous). Greedy
  picks one; log the runner-up to the **review queue**. Do **not** auto-merge.
- **Backfill can bridge** two previously-disjoint clusters (a middle year linking
  an early and a late cluster).

Policy: **never silently rename an existing EMP_ID.** When a merge is warranted,
the **older anchor wins** (survivor = earliest first-seen); the retired ID is
recorded in an `id_crosswalk` (`retired_id → survivor_id`) so downstream can
follow it. Splits are review-only, not automatic.

## 6. API surface (to build after this plan is approved)

- `emp_anchor_key(df, cols)` — deterministic-earliest anchor per cluster (§3.2).
- `anchor_emp_id(anchor_key)` — hash → `EMP_ID` (with version tag).
- `match_to_profiles(new, profiles, ...)` — the block+score+assign core (steps
  2–5); reused by wave-matching and callable standalone.
- `link_incremental(existing, new, ...)` — the full pipeline (steps 1–6).
- `remint_anchored(linked)` — one-time migration: add `EMP_ANCHOR` and re-mint
  `EMP_ID` on an old membership-hash panel; emits the old→new crosswalk.

## 7. Reuse map (grounding)

| Need | Existing function |
|---|---|
| Profiles | `build_person_profile` (`R/person_profile.R`) + EMP_ANCHOR passthrough |
| Blocking | `person_blocking_keys` / `candidate_pairs` (`R/cross_org.R`) or org-block |
| Set scoring | `.best_set_sim`, `score_pairs`, `compare_last_names`, `compare_first_names` |
| Within-org surname freq | `surname_weight` (`R/score.R:52`) |
| One-to-one | `greedy_one_to_one` (`R/link.R:274`) |
| Residual clustering | `link_panel` on the residual subset |
| Anchor key | `person_year_id` (`R/id.R:85`) |

## 8. Open questions
1. Wave granularity — always one tax year, or allow multi-year waves? (Step 6
   already tolerates multi-year residuals.)
2. Should `link_incremental` return the merged full panel, or only the stamped
   new rows (leaving the append to the caller)? Lean: stamped new rows +
   crosswalk; caller `rbind`s.
3. Re-tune `threshold` for the asymmetric row-vs-profile score, or inherit the
   within-org default (7)? Needs a labeled wave slice, like `dev/tune_threshold.R`.
4. Cross-org refresh after a wave — incremental, or a full `link_cross_org`
   re-run on affected orgs?
