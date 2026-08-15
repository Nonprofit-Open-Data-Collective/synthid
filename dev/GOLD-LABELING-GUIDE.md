# Gold-labeling guide — `gold_sample_template.csv`

Fill **`gold_role`** with ONE value from the controlled vocabulary below (the
person's TRUE role, your judgment — this is the benchmark the cascade is scored
against). Use **`gold_notes`** for dual roles, transitions, or uncertainty.
Allowed values are also in `gold_role_allowed.csv` (use as an Excel dropdown).

## Controlled vocabulary (`gold_role`)

**Leadership**
- `CEO` — the org's chief executive: a titled CEO / President-CEO / Executive
  Director, OR the de-facto paid operational head when there's no formal CEO title.
- `CO_CEO` — one of 2+ concurrent chief executives (true co-leaders, or overlapping
  outgoing+incoming in a transition year).
- `INTERIM_CEO` — explicitly interim / acting chief executive.

**Paid officers / senior staff**
- `CFO` — chief financial officer / finance head.
- `COO` — chief operating officer / operations head.
- `C_LEVEL_OTHER` — other C-suite: CIO, CMO, CHRO, chief X officer, general counsel.
- `OFFICER` — a paid senior officer/leader not clearly C-level (e.g. a working VP).

**Board (governance, usually unpaid)**
- `BOARD_CHAIR` — board president / chair / chairman.
- `BOARD_OFFICER` — board secretary, treasurer, or vice-chair (board member in an officer post).
- `BOARD_MEMBER` — regular trustee / director / council member / delegate.

**Staff**
- `MANAGER` — mid-level manager / program director / department head (authority, not senior leadership).
- `KEY_EMPLOYEE` — highly-compensated key employee flagged by the filer, non-officer.
- `STAFF` — rank-and-file / professional / specialist (doctor, teacher, analyst) with no leadership or governance role.

**Special**
- `DUAL` — genuinely two roles (e.g. board member AND paid ED). Put both in `gold_notes`, e.g. `BOARD_MEMBER + CEO`.
- `UNSURE` — cannot determine from the available evidence.
- `NONE` — not a real person/title (garbage, blank, placeholder).

## Decision rules (use the evidence columns)

1. **Checkboxes first.** `cb_tru` (trustee) → lean board; `cb_off` (officer) → lean
   officer/leadership; `cb_key` (key/high-comp) → lean key staff. They can co-occur
   (working board) → often `DUAL`.
2. **Comp + hours cross-check.** unpaid + low hours → board; paid + high hours → staff/officer.
3. **Title breaks ties.** exec (CEO/ED/President-as-exec) → `CEO`; C-suite → `CFO`/`COO`/`C_LEVEL_OTHER`;
   governance words (trustee/board/chair/council/delegate) → board.
4. **"President" specifically.** paid + officer box + meaningful comp → `CEO`;
   unpaid + trustee box → `BOARD_CHAIR`.
5. **dir.vp cases** (the hard bucket): trustee box / unpaid → `BOARD_MEMBER` or `BOARD_OFFICER`;
   paid officer with real authority → `OFFICER` / `C_LEVEL_OTHER`; paid mid-level → `MANAGER`.
6. **board-governed orgs.** If the paid people are support-level, label them `STAFF`/`MANAGER`/`KEY_EMPLOYEE`
   — do NOT invent a CEO. Only label `CEO` if one person clearly runs operations.

## Bucket-specific hints (the `bucket` column tells you what to scrutinize)

- `chief_staff_imputed` — **the key check**: is this person really the operational head
  (→ `CEO`) or just the top-paid staffer in a board-run org (→ `MANAGER`/`STAFF`)?
- `board_governed_org_paid` — expect `STAFF`/`MANAGER`/`KEY_EMPLOYEE`; confirm no hidden CEO.
- `board_box_only` — filer checked trustee but the title isn't obviously board → usually
  `BOARD_MEMBER` (trust the filer's box).
- `board_title_only` — board-ish title but no trustee box → `BOARD_MEMBER`, or a paid officer if comp says so.
- `dir.vp_ambiguous` — decide board vs `C_LEVEL_OTHER` vs `MANAGER`; this bucket calibrates the unravel.
- `co_ceo` — confirm concurrent vs transition (note `outgoing`/`incoming` in `gold_notes`).

## `gold_notes` conventions
- `DUAL`: list both roles (`BOARD_CHAIR + CEO`).
- transitions: `outgoing` / `incoming`.
- anything you found decisive or that made it hard (helps us tune the rules).
