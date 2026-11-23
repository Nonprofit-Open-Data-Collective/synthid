# First-name variant table (draft)

A table of pairs of first names that are two forms of one given name, built
from the names one person (EMP_ID) carries across their own organization's
filings in the national Part VII v3.1 build. It is meant to replace
`nickname_table()` (70 formal names), which covers 48% of the variant pairs
seen in the data; this draft covers the pairs seen in 25+ EMP_IDs.

It is called a *variant* table, not a nickname table, because only 40% of its
pairs are nicknames. "Disambiguation" would suggest the opposite job (telling
people apart); this table says which forms are the *same* name.

## Scripts

The vignette `vignettes/first-name-variants.Rmd` describes the build for
readers; this file is the working record.

| Script | Does | Writes |
|---|---|---|
| `00-variant-pairs.R` | every pair of first-name forms within an EMP_ID, short form first, counted by EMP_ID | `SYNTHID_DIR/partvii-v3_1/first-name-variant-pairs.csv` |
| `00b-compare-tables.R` | first-pass coverage by the three nickname tables (superseded by 02) | `SYNTHID_DIR/.../first-name-variant-pairs-vs-tables.csv` |
| `01-pair-evidence.R` | co-filing counts (the two forms as separate people with one surname on one return) and the gender recorded with each first name | `SYNTHID_DIR/partvii-v3_1/name-variants/` |
| `02-classify.R` | types each pair (rules plus the hand review of the 25+ pairs, as lists in the script), checks it against the public tables, writes the files below and the review sample | this folder; candidates to `SYNTHID_DIR/.../name-variants/` |
| `03-review-first-pass.R` | first-pass review calls on `review-sample.csv` and error rates by stratum | `review-sample.csv` |
| `04-sound-alikes.R` | sound-alike spellings among the under-25 candidates | `first-name-sound-alikes.csv` |
| `05-chains.R` | chains (connected components) and chain columns; vignette figures | chain columns in `first-name-variants.csv`, `first-name-variant-chains.csv`, `vignettes/figures/name-chains-*.png` |

All run from the package root, in order (rerun 05 after 02, since 02 rewrites
the table). The public tables are copied in `reference/`:
`carltonnorthern.csv` (github.com/carltonnorthern/nicknames, Apache-2.0) and
`onyxrev.csv` (github.com/onyxrev/common_nickname_csv); 02 reads them from
there unless `NICKNAME_TABLES_DIR` is set.

## Variant types

| Type | Meaning | Examples | In table |
|---|---|---|---|
| nickname | a different word for the name, incl. clipped short forms and diminutives | BOB/ROBERT, CHRIS/CHRISTOPHER, KATIE/KATE | yes |
| spelling | the same name spelled another way | JEFFERY/JEFFREY, STEVEN/STEPHEN, SEAN/SHAWN | yes |
| compound | one element of a compound given name | MARY/MARYANN, ANN/ANNMARIE | yes |
| cognate | the same name in another language | JOSE/JOSEPH, RICARDO/RICHARD | yes |
| typo | a misspelling of one name; a record error, which the scorer's string distance already handles | JOSPEH/JOSEPH, BRAIN/BRIAN | errors file |
| parse_error | not a first name | JP/JPMORGAN, WILLIAM/WILLIAMS | errors file |
| questionable | likely two different names or people | JOHN/JOSH, LISA/LINDA, PAUL/PAULA | questionable file |

Truncations are nicknames for matching (CLINT/CLINTON works like BOB/ROBERT).
The "prefix" group in the first pass was not one type: it mixed short forms
(CLINT/CLINTON), spellings (GREG/GREGG), compounds (MARY/MARYANN), other
genders (PAUL/PAULA, JOE/JOEL) and typos (ROBER/ROBERT). A prefix rule alone
would accept all of them.

## Files

| File | Pairs | What |
|---|---|---|
| `first-name-variants.csv` | 1,007 | the draft table: 25+ pairs of the four kept types, 95% of 25+ EMP_ID rows |
| `first-name-variants-questionable.csv` | 83 | hand review: likely different names or genders |
| `first-name-variants-errors.csv` | 187 | typos (179) and parse errors (8) |
| `review-sample.csv` | 212 | stratified sample for the error-rate check, with first-pass calls |
| `first-name-sound-alikes.csv` | 23,965 | under-25 pairs whose spellings sound alike (strict key) |
| `first-name-variant-chains.csv` | 323 | one row per chain: size, pairs, unlinked pairs, diameter, hub |
| `SYNTHID_DIR/.../first-name-variants-candidates.csv` | 52,467 | pairs under 25, with a verification gate |

Columns: `n_emp_ids` (people carrying the pair), `in_synthid/in_carlton/in_onyx`
(listed in that table, directly or through a shared formal name),
`n_filings_together` and `cofiling_rate` (per EMP_ID), `freq_*` (rows
nationally with that first name), `f_*` (share recorded female), `flags`.

`flags` on a kept pair is a warning, not an error:
- `cofiling` (64 pairs): the forms also appear as two people on one return,
  mostly diminutives that families use for relatives (JOHN/JOHNNY,
  JIM/JIMMY, DON/DONNIE; Sr and Jr). Real variants, weaker evidence.
- `gender` (16): unisex short forms (PAT, JAMIE, RENE). The gender field is
  inferred from the first name, so it cannot settle these.

## How many are errors

1. **Evidence screen (done).** Co-filing separates the groups: median rate
   0.017 for nicknames, 0.006 for spellings, 0.088 for the hand-marked
   questionable pairs, and 48% of questionable pairs flagged against 11% of
   nicknames.
2. **Stratified review (first pass done, needs a person's check).**
   `review-sample.csv` holds up to 40 random pairs per type and per
   promote-ready candidate gate, marked `same_name` Y/N/? by Claude
   (`03-review-first-pass.R`). Error rate, counting ? as half an error:

   | Stratum | n | Y | ? | N | Error | 95% upper |
   |---|---|---|---|---|---|---|
   | main: nickname | 40 | 39 | 1 | 0 | 1.3% | 10.9% |
   | main: spelling | 40 | 37 | 3 | 0 | 3.8% | 14.7% |
   | main: cognate | 32 | 28 | 4 | 0 | 6.2% | 20.1% |
   | main: compound | 20 | 20 | 0 | 0 | 0% | 16.1% |
   | ready: rule + support | 40 | 33 | 4 | 3 | 12.5% | 26.1% |
   | ready: in a public table | 40 | 33 | 7 | 0 | 8.8% | 21.5% |

   The doubts in the table are mostly cognates (MARIA/MARIE, JOSE/JOSEPH,
   ANDRE/ANDREW). The candidate gates have about twice the table's error rate;
   the clear errors are distinct names that sound alike (BRENNA/BREANNA,
   DINA/DIANA, LAN/LANA). Do not promote the gates as they stand.
3. **Downstream.** Swap the table into `compare_first_names()` and rerun
   06j (seeded recall) and 06k (false-positive calibration).

## Pairs under 25

52,467 pairs (96,507 EMP_ID rows), kept in the candidates file with a gate:

| Gate | Pairs | Rule |
|---|---|---|
| ready: in a public table | 548 | listed in carltonnorthern, onyxrev or synthid |
| ready: rule + support | 779 | spelling, compound or cognate rule, 5+ EMP_IDs, both forms 100+ rows nationally |
| hold: red flag | 2,863 | co-filing or gender flag |
| hold: needs review | 26,968 | no rule or table support |
| excluded: rule | 21,309 | typo, parse error or a questionable pair |

Promote a gate when its review-sample stratum passes. Most "needs review"
pairs are seen once (41,465 pairs overall appear in one EMP_ID) and add
little; they can stay out.

## Sound-alike spellings (phonetics list)

`04-sound-alikes.R` tests the 50,153 under-25 pairs of alphabetic names with
a short form of 3+ letters. Soundex agrees on 30,554, but it also merges
different names (DAN/DON, TIM/TOM, JAN/JEN). The strict key normalizes
spelling to sound (PH=F, V=F, C=K or S, Y=I, silent H, doubled letters, final
-IE/-EY), keeps the first vowel exactly, and drops later vowels. It agrees on
23,965 pairs (47,331 EMP_ID rows):

| Kind | Pairs | EMP_ID rows | Seen 2+ |
|---|---|---|---|
| sound-alike spelling | 11,806 | 14,879 | 1,602 |
| misspelling that sounds right (typo rule) | 10,217 | 22,005 | 3,270 |
| sound-alike with a red flag | 1,173 | 2,894 | 504 |
| already promote-ready | 769 | 7,553 | 733 |

This list fits a phonetic blocking or fuzzy key better than the variant
table. Sounding alike does not make two forms the same name: JEREMY/JEREMIAH,
ALEXA/ALEXIS and BRAD/BRADY pass the key. The strict key also misses some real
pairs that Soundex catches, such as clipped forms (CHRISTIE/CHRISTINA) and
transpositions (BIRAN/BRIAN).

## Chains

Each pair links two names, so the table forms a network. `05-chains.R` finds
its connected components ("chains"): 1,007 pairs connect 1,166 names into 323
chains. 157 chains are a single pair, 106 have 3-4 names, 45 have 5-9 and 15
have 10+. The largest:

| Chain | Names | Pairs | Unlinked name pairs | Hub |
|---|---|---|---|---|
| 1 | 27 | 34 | 317 | AL (ALAN, ALBERT, ALFRED, ALEXANDER, ...; BERT joins ROBERT) |
| 2 | 24 | 25 | 251 | ELIZABETH (and ELEANOR/ELLEN/EILEEN through ELLIE) |
| 3 | 24 | 40 | 236 | KATHERINE / CATHERINE / KATHLEEN / KATHRYN |
| 4 | 23 | 35 | 218 | CHRISTINA / KRISTIN / KIRSTEN |
| 5 | 14 | 15 | 76 | MARGARET (and MEGAN through MEG) |

Columns added to the table: `chain_id` (1 = largest), `chain_names`,
`chain_pairs`, `chain_unlinked` (name pairs in the chain that are not listed
pairs: the matches a transitive rule would add). The unlinked counts are the
case against matching by chain.

## Storing the table: pairs, not canonical names

A table that maps each name to one canonical form (RICK -> RICHARD) breaks on
names with more than one root: KATE (KATHERINE, KATHLEEN, CATHERINE), AL
(ALBERT, ALAN, ALFRED), PAT (PATRICK, PATRICIA), RICKY (RICHARD, RICARDO).

The table stores pairs, and the comparator should test a pair directly, never
through chains. If names were merged into clusters (connected components),
KATHLEEN = KATE = KATHERINE would make KATHLEEN match KATHERINE, and
RICHARD = RICKY = RICARDO would make two names match through a shared short
form. `nickname_table()` already avoids this. Two names match when they share
a formal root, and two formal names never share one. That is why
KATHLEEN/KATHERINE is in the questionable file.

Cognates are a separate type, so the scorer can weight them less.
RICKY/RICARDO and RICKY/RICHARD are both nicknames. RICARDO/RICHARD is a
cognate: true for some people (an anglicized name) and false for others.
