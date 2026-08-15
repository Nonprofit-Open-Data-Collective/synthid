# Dev plan — title role-refinement: targeted accuracy + segment models

Objective: turn the raw title classifier into a role-resolver whose accuracy we
can *measure per category* and improve segment by segment. Prototyping in
`synthid/dev`; target home is `titleclassifier` step 09 (`conditional_logic`).

## Category tiers (what we optimize vs. what we only assess)

| Tier | Categories | Why |
|---|---|---|
| **OPTIMIZE** (hard target) | CEO / co-CEO / interim; board membership + board count | Filer checkboxes + comp give (proxy) ground truth; these drive comp-study reconciliation |
| **ASSESS** (reliability, may re-strategize) | c.level, dir.vp, mgr, spec | No direct checkbox truth; decide per category whether to keep, split, or collapse to a coarse **KEY / "important"** label |
| **REPORT** (coverage only, not optimized) | board president, board secretary, board treasurer | Not required to file; report presence/duplication, don't chase accuracy |

## Scorecard (tracker: `09_role_scorecard.R` -> `role_scorecard.csv`)

Baseline on full-990 x paid (1,381 org-years). Re-run after every refinement.

| Tier | Category | Metric | Baseline | Target |
|---|---|---|---|---|
| OPTIMIZE | CEO | paid orgs with >=1 CEO | 0.966 | **1.00** |
| OPTIMIZE | CEO | CEO persons corroborated | 0.959 | >=0.95 |
| OPTIMIZE | CEO | no-CEO paid orgs (chief-mgr gap) | 47 | **0** |
| OPTIMIZE | BOARD | recall vs trustee box* | 1.000 | >=0.98 |
| OPTIMIZE | BOARD | org board-count exact | 0.979 | >=0.90 |
| ASSESS | c.level | %paid / %officer-box | 0.94 / 0.82 | coherent -> keep? |
| ASSESS | dir.vp | %paid / %officer-box / med comp | 0.55 / 0.62 / **$8,986** | **incoherent -> re-strategize** |
| ASSESS | mgr | %paid / %officer-box | 0.96 / 0.46 | small (n=107) |
| ASSESS | spec | %paid / med comp | 0.84 / $120k | coherent -> KEY? |
| REPORT | board.president | orgs with exactly 1 | 0.598 | report (30% have >1) |
| REPORT | board.secretary | orgs with exactly 1 | 0.647 | report |
| REPORT | board.treasurer | orgs with exactly 1 | 0.532 | report |

\* **Measurement caveat**: the cascade assigns board *from* the trustee box, so
recall/precision vs that box is partly circular. Board accuracy needs an
INDEPENDENT truth: (a) title-based board detection for unpaid board members with
no checkbox (catches crosswalk gaps like REGULAR TRUSTEE), and (b) a hand-labeled
sample. Build both before trusting the board numbers.

## Identities (locked 2026-08-14)

- **Board president/chair is NOT capped at one per org.** Multiples are real:
  transition (outgoing+incoming) or shared/concurrent. Annotate, never collapse.
  Baseline: 860 single / 272 shared-concurrent / 215 transition.
- **Leadership is three-way**, and imputed leaders are flagged distinct from real
  CEOs: `ceo_designated` (title) / `co_ceo` / `interim_ceo` / `chief_staff_imputed`
  (no designated CEO but a plausible paid lead — flagged) / **`board_governed`** (no
  designated CEO and no plausible paid lead -> NO CEO crowned; the board runs it).
  Baseline orgs: 793 designated / 518 chief_staff_imputed / 70 board_governed.
  => "CEO coverage" target is NOT 100%: board_governed is a valid outcome, not a miss.
- **dir.vp unravel order**: (1) partition board members out, (2) route the residual
  UP to c.level or DOWN to manager — largely via the crosswalk.

## Classifier passes (stub module: `10_role_passes.R`)

One pass per category; run in order. Status: [x] impl, [~] partial, [ ] stub.

1. `[x] pass_resolve_leadership` — designated/co/interim CEO + chief_staff_imputed
   (flagged) + board_governed (no-CEO). Replaces the old flag_chief_manager +
   identify_ceo. **The "First" ask, with the board-run guard.**
2. `[x] pass_annotate_board_officers` — tag board pres/sec/treas multiples as
   outgoing / incoming / interim / shared-concurrent. NO one-per-org cap.
3. `[~] pass_capture_board` — trustee box + board title + crosswalk patch (08);
   independent title-based truth now in `11_board_truth_and_gold.R`.
4. `[x] pass_unravel_dirvp` — partition board out, then route non-board residual
   up (officer/c-level) / down (manager) by title. 1,688 dir.vp -> 626 board /
   615 up / 253 down. Writes `dirvp_crosswalk_suggestions.csv` (VP OF [function]
   -> officer; DIRECTOR OF [dept] / plain DIRECTOR -> manager) to migrate into
   the crosswalk. Runs after add_role_coarse; never overrides a resolved CEO/BOARD.
5. `[ ] pass_assess_finegrained` — c.level/spec coherent (keep); mgr small;
   reliability tag + coarse role.

The ASSESS passes don't relabel yet — they attach a `reliability` tag and a
proposed `role_coarse` (CEO / BOARD / OFFICER / KEY / STAFF) so we can ship a
trustworthy coarse layer even where the fine label is shaky.

## Segment model derivation (train on clean 990/paid, transfer outward)

Signals available per segment decide the model:

| Segment | Checkboxes | Pay | Strategy | Priority |
|---|---|---|---|---|
| **1. Full-990 x paid** | yes | yes | **Rule cascade** (checkbox+comp+title). Produces silver labels. DONE (06). | done |
| **2. Full-990 x unpaid** | yes | no | Model on **title + checkbox** (drop pay features); labels transferred from seg-1 rules on the checkbox-only feature subset | med |
| **3. 990EZ x paid** | no | yes | Model on **title + hours + pay** (no checkbox); labels from seg-1 + the **EZ->990 transition trick** (person's next-year full-990 checkboxes) | high (comp studies need EZ) |
| **4. 990EZ x unpaid** | no | no | **Gentle pass**: title-only heuristic; these orgs usually dropped from comp studies | low |

Derivation flow:
1. Seg-1 rule cascade -> silver labels {CEO, BOARD, OFFICER, C-LEVEL?, KEY, STAFF}
   on 22k persons. Validate a hand-labeled sample -> gold.
2. Train two feature-restricted models on seg-1 gold:
   - **M_nopay** (title + checkbox) for seg-2.
   - **M_nobox** (title + hours + pay) for seg-3 — augment with EZ->990 transition
     labels (see `NOTES-title-role-disambiguation.md`).
   Model class: gradient-boosted trees (tabular features + title embeddings). Only
   pursue where rules plateau — for seg-1 rules already hit ~97% on CEO.
3. Apply models to their segments; keep the coarse {CEO/BOARD/OFFICER/KEY/STAFF}
   layer as the guaranteed output, fine labels where confident.

## Phases & checkpoints (each gated by the scorecard)

- **P1 (now)** — chief-manager pass -> CEO coverage 1.00; board-officer dedup;
  independent board-truth metric + 150-row hand-labeled gold sample.
- **P2** — ASSESS dir.vp/c.level/mgr/spec; decide KEY-collapse; freeze the coarse
  role layer.
- **P3** — port seg-1 cascade into `titleclassifier` step 09 (behind regression).
- **P4** — train M_nopay / M_nobox; apply to seg-2 / seg-3; EZ transition labels.
- **P5** — gentle seg-4 pass; final scorecard across all four segments.
