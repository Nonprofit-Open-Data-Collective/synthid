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

**Batch vs. incremental boundary (found in Phase 1 implementation).** A
from-scratch `link_panel()` rebuild has no memory of when a person was *first
seen* — it picks the deterministic-earliest of whatever cluster it sees. So:
- **Forward waves are stable even under a full batch re-run** — the earliest
  record is unchanged, so the anchor (and id) don't move. This is a free property
  of deterministic-earliest anchoring and covers the common case.
- **Backfill (an earlier record) re-anchors under a batch re-run** — correctly, by
  the batch rule. Freezing the anchor across a backfill is therefore a property of
  the **incremental path** (`link_incremental`, Phase 3), which carries the prior
  `EMP_ANCHOR` forward and only mints for genuinely new persons. Phase 1
  (`link_panel`) stays pure batch; no carried-anchor logic.

> Change `assign_emp_ids()` so the *initial build* uses anchored IDs too, so
> `link_panel` and `link_incremental` share one recipe. Bump a `keyspec`-style
> version tag on the ID so pre/post-migration IDs can never silently collide.

> **STATUS: Phases 1–3 implemented** (branch `feat/incremental-waves`). Anchored
> ids (`R/anchor.R`), the matcher core (`match_to_profiles`, `R/incremental.R`),
> and the full pipeline (`link_incremental`, `R/incremental.R`) are done and
> tested (`tests/testthat/test-anchor.R`, `test-incremental.R`). Remaining:
> threshold tuning (OQ #3) and cross-org refresh (§7 / OQ #4).

## 4. The `link_incremental()` pipeline (unified single/multi-year)

The wave may span one year or many. Rather than special-casing, **link the wave
internally first, then match wave-persons to existing persons** — single-year is
just the case where every wave-local person has ≤1 record per org-year. This
turns the whole thing into profile-vs-profile matching, which the cross-org stage
already does.

Inputs: `existing` (a linked panel carrying `EMP_ID`, `EMP_ANCHOR`, name
components, org, year, and — for anchoring — OBJECTID/TABLE_ID) and `new`
(parsed wave, same schema, no IDs yet).

1. **Profile the existing panel** → `build_person_profile(existing)` (extended to
   pass `EMP_ANCHOR` through). One profile per existing person. Cacheable between
   waves (parquet/rds), dovetailing with the DuckDB batching workflow.
2. **Link the wave internally** → `link_panel(new)` gives the wave its own
   provisional within-org clusters. This is what absorbs the multi-year case (a
   first-time person appearing in 2022+2023+2024 becomes ONE wave-person).
   `build_person_profile(wave_linked)` → one **wave-profile** per wave-person.
3. **Block wave-profiles → existing profiles, same org** (optionally + surname
   key), mirroring the cross-org hash-block (`person_blocking_keys` /
   `candidate_pairs`).
4. **Score** each (wave-profile, existing-profile) candidate with the cross-org
   set-scorer: `.best_set_sim()` over the variant sets with `compare_last_names` /
   `compare_first_names`, `score_pairs()`. **Surname frequency: population rarity**
   (`population_surname_weight()` over the merged profile set) — *revised from the
   original within-org note during Phase 2.* At the match stage both people are
   already distinct identities (wave-internal linkage did the family-board
   separation), so the surname weight only calibrates how surprising the agreement
   is *across the wave boundary* (`SMITH` weak, `GANTSOUDES` strong) — exactly the
   cross-org question. Overridable via `surname_weight=`.
5. **Accept** edges ≥ `threshold`, **greedy one-to-one** (`greedy_one_to_one()`),
   under the **one-record-per-(org,year) invariant across the merged timeline**:
   a wave-person may merge into an existing person only if their `(org, year)`
   footprints are **disjoint** (same guard as `resolve_clusters`, generalized from
   year to (org,year) keys). A collision ⇒ they are different people ⇒ reject.
6. **Resolve IDs:**
   - wave-person matched to an existing person ⇒ **inherit** that `EMP_ID` /
     `EMP_ANCHOR`;
   - wave-person matched nothing ⇒ **mint** a fresh anchored ID from its earliest
     wave record (§3).
   Then stamp every new record via its wave-local cluster.
7. **(Follow-on, out of scope for v1)** re-run `link_cross_org` for affected orgs
   — new interlocks can appear.

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
- `match_to_profiles(wave, existing, ...)` — the block+score+assign core (steps
  3–5); reused by wave-matching and callable standalone. **DONE (Phase 2,
  `R/incremental.R`)** with `same_org_candidate_pairs()` blocker; returns
  `matched` / `unmatched` / `review` (ambiguous + invariant-collision) / `report`.
- `link_incremental(existing, new, ...)` — the full pipeline (steps 1–6).
  **DONE (Phase 3, `R/incremental.R`)**: profiles existing → links wave internally
  → matches → stamps `new_stamped` with inherited/fresh anchored ids; drops exact
  `(OBJECTID,TABLE_ID)` re-loads with a warning; returns
  `new_stamped`/`review`/`unmatched`/`wave_id_map`/`report`. Backfill-freeze is
  delivered here (matched wave-person inherits the existing anchor). No automatic
  merges — ambiguous second-matches go to `review`, never renamed.
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
1. ~~Wave granularity~~ **Resolved (2026-08-14): support single AND multi-year.**
   Handled by linking the wave internally first (§4 step 2), so both collapse to
   profile-vs-profile matching.
2. Should `link_incremental` return the merged full panel, or only the stamped
   new rows (leaving the append to the caller)? Lean: stamped new rows +
   crosswalk; caller `rbind`s.
3. ~~Re-tune `threshold`~~ **Resolved (2026-08-15): keep 7, confirmed.**
   `dev/tune_match_threshold.R` holds out 2021 as a wave against a full-panel
   oracle; post-greedy P/R are flat across 5–8 (greedy one-to-one carries the
   accuracy, not the cutoff), max-F1 at 7.5. Knob stays exposed. See
   `dev/NOTES-match-threshold.md`.
4. Cross-org refresh after a wave — incremental, or a full `link_cross_org`
   re-run on affected orgs?
