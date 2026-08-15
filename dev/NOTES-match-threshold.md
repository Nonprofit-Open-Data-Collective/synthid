# Match-threshold tuning — result & decision

Tuner: `dev/tune_match_threshold.R`. Data: `PANEL-2019-2021-W-NAMES-TITLES-AND-NAMES.CSV`
(16,912 rows, 500 EINs). Oracle: full-panel `link_panel()` truth ids; hold out
2021 as the wave, link 2019–2020 (existing) and 2021 (wave) separately, match.

## Headline: the threshold is a weak lever; greedy one-to-one does the work

Post-one-to-one operating points (the real decision) are **flat**:

| thr | accepted | precision | recall |
|----:|---------:|----------:|-------:|
| 5 | 4580 | 0.938 | 0.939 |
| 6 | 4579 | 0.938 | 0.939 |
| 7 | 4573 | 0.938 | 0.938 |
| 8 | 4567 | 0.938 | 0.937 |

Pair-level max-F1 is at **7.5** (P=0.881, R=0.998). Precision only climbs above
~0.88 at thr ≥ 12, and only by sacrificing most recall (thr 12: P=0.908, R=0.74).
So within 5–8 the choice barely moves accuracy — the greedy one-to-one selection
(one match per person, best score wins) is what secures precision, not the cutoff.

## Decision: keep `match_threshold = 7`

Confirmed, not changed. It sits at max-F1 (7.5), matches the within-org linker's
default (one fewer magic number), and lands in the middle of the flat 5–8 plateau.
The knob is exposed on `match_to_profiles()` / `link_incremental()` for callers who
want a different precision/recall trade (raise toward 12 for near-1.0 precision at
heavy recall cost; lower toward 5 to admit more borderline variants).

## Two caveats that matter more than the exact number

1. **The precision figure (~0.94) is a floor, depressed by an oracle artifact.**
   The panel carries *multiple title-rows per person per org-year*. The within-org
   one-record-per-(org,year) invariant cannot place two same-year rows in one
   cluster, so it fragments a multi-title person into several clusters. The oracle
   inherits that fragmentation and labels many *identical-name* candidate pairs
   (e.g. nine `BILL GASSEN`↔`BILL GASSEN`) as "different people". These are the
   bulk of the flagged false positives (`dev/review_match_false_positives.csv`) —
   an artifact of the label, not matcher errors. True precision is higher.

2. **The real misses are OCR/nickname variants just under threshold.** The clean
   false negatives (`dev/review_match_false_negatives.csv`) are genuine same-person
   pairs scoring 5.3–6.9: `CATHY`/`CATHLEEN GRAHAM`, `JAK`/`JAKE JAFFE`,
   `CYNTHIS`/`CYNTHIA ADKISSON` (typo), `JUSTON`/`JUSTIN SABOL`, `STANDFORD`/
   `STANFORD`, `ELEARNOR`/`ELEANOR DUNN`. A person who only misses by the cutoff
   gets a *new* id (fragmentation), not a wrong merge. Lowering the threshold to
   ~5–6 recovers a handful of these with no measured precision cost (greedy
   protects it) — a reasonable choice for a fragmentation-averse run; left at 7 by
   default because the absolute count is tiny and 7 keeps parity with link_panel.

## Follow-ups
- Better oracle: dedup to one row per person-org-year (or link on the
  person-year key) before taking truth ids, to remove the multi-title artifact and
  get a trustworthy precision curve.
- Hand-label a sample of the identical-name FPs to confirm they are the artifact.
