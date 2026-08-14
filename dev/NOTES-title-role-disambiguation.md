# Title role-disambiguation — development notes

Context: resolving ambiguous IRS 990 Part VII titles (president, director, chair,
deputy, and odd board names) into the correct LEVEL — board governance vs. paid
officer/leadership vs. paid staff. Prototyping lives in `synthid/dev`; target home
is `titleclassifier` step 09 (`conditional_logic`, currently a 2-rule stub).

## Why a rule cascade, not (yet) an ML model — for Full 990 × Paid

The title→role crosswalk (`titleclassifier/data-dev/title-taxonomy-map.xlsx`) is by
design a **strict 1:1 map** (`instructions` sheet: "each title occurs only once",
"all title=X should have the same taxonomy codes"). A 1:1 map **cannot** disambiguate
context-dependent titles. The crosswalk authors hit this themselves — `notes` sheet:
> "it's sometimes ambiguous when 'president' refers to ceo and when it refers to
> board pres" → rule: "if president = officer and paid, or former and paid ⟹ ceo"
and `title-standardization` row 1 carries `*if no officer flag`. They knew the
conditional logic was needed but had nowhere to put it. That logic belongs in the
step-09 cascade.

Measured determinism on the full-990×paid cohort (1,381 org-years, 22,249 rows):
- 98% of rows carry a checkbox signal (only 395 have none).
- 497 org-years have paid staff but NO exec labeled; **496 are deterministically
  recoverable** (452 have a paid officer to promote, 403 working boards, 44 paid-board-
  no-officer) — only 1 truly ambiguous.
- Among orgs with paid officers, 75% have a unique top-paid officer (clean CEO pick);
  ties are genuine co-CEO / transition-year overlaps, not failures.

So the errors are *caused* by a static default rule ignoring structured fields; the
cure is a better rule, not a learned model. ML earns its place only where rules
plateau: the 990EZ segments (no checkboxes) and the free-text residual.

Caveat that shapes the cascade: the officer box is strong but **imperfect** — 18.6% of
unambiguous exec titles (EXECUTIVE DIRECTOR, COO, CEO, CFO...) lack the officer box.
Never trust the checkbox alone; combine checkbox + comp + title.

## Refinement order (decided)

1. **Cascade (Layer 1 position / Layer 2 function) first** — only place context logic
   can live; unblocks the crosswalk from encoding hacks.
2. **Crosswalk refinement second**, frequency-weighted by the cascade residual + the
   ~2,259 NA titles (context-free errors: misspellings, COMPTROLLER→c-level, to-do queue).
3. **ML last**, for 990EZ + free-text residual (needs labels steps 1–2 generate).

## IDEA — build the 990EZ ML training set from EZ→full-990 filing transitions

990EZ returns omit the Part VII position checkboxes, so the EZ segment has no direct
board/officer signal — the hardest segment and the main reason for a separate model.

Proposal: use orgs that **switch from filing 990EZ to a full 990** across adjacent years
(detectable via synthid's `EMP_ID` panel + `formtype`). For a person present in both:
- **Year t (990EZ)** = the feature row we want to predict (title text, comp, hours only).
- **Year t+1 (full 990)** = supplies the LABEL — the officer/trustee/key-employee
  checkboxes — assuming the person's role is stable across the one-year gap.

This yields a labeled EZ-shaped training set "for free" (self-supervised via the panel):
predict the year-t role from EZ-available features, supervised by the year-t+1 checkboxes
of the same linked person. Same trick works reversed (full→EZ) to validate stability.

Cautions to handle when building it:
- Restrict to `EMP_ID`s with a genuine role match across the boundary (guard against
  actual promotions/turnover in the gap — the transition year is exactly when roles change).
- Weight by how stable the role is across all observed years for that person.
- Org size/structure can change with the EZ→990 switch (often growth) — the role mix may
  shift, so treat labels as noisy.
- Confirm the checkbox fields are reliably populated in the destination full-990 year.

Cohort sizing (how many EZ→990 switchers exist in the panel) is a TODO before committing.
