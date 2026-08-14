# Post-nominal credentials and glued honorifics leak into parsed name fields

## Summary

A downstream QA pass in **synthid** (`parse_fail_log()`, run over a 33,984-record
2010–2024 Form 990 Part VII slice) surfaced 32 records whose parsed name
components carry a recoverable, syntactic parse error. They fall into three
classes:

1. **Post-nominal credential tokens leak into `last_name`** — 10 distinct
   abbreviations, **all absent from `known_titles()`**.
2. **A personal honorific fused to the given name with no space** (`MRWILLIAM`)
   is not split, even though `MR` *is* in `known_titles()`.
3. **A single stray initial fused to the surname** (`CMARTIN` → `MARTIN`).

Each was found by cross-year consensus: the same linked person appears in other
years with the clean parse, which both proves the defect and supplies the
expected value. Full record-level log (input → wrong parse → expected, with
`OBJECTID`/`TABLE_ID` keys) attached as `peopleparser_parse_fails.csv`.

---

## 1. Missing post-nominal credentials (→ `known_titles()`)

These tokens survived parsing inside `last_name` (e.g.
`"Salvatore Martino EdD RTR FASRT CAE"` → `last_name = "RTR-FASRT-MARTINO"`).
Cross-checked against `known_titles()$abbr`: **none of the following are present.**
Recognized post-nominals on the same records (e.g. `EdD`, `CAE`) *were* correctly
moved to `salutation`, so completing the list should close the gap.

| abbr | count | proposed `label` | proposed `type` |
|------|------:|------------------|-----------------|
| `RTR`   | 6 | Registered Technologist (Radiography) | Professional Credential |
| `FASRT` | 4 | Fellow, Am. Society of Radiologic Technologists | Professional Credential |
| `RDMS`  | 4 | Registered Diagnostic Medical Sonographer | Professional Credential |
| `RVT`   | 4 | Registered Vascular Technologist | Professional Credential |
| `LCSW`  | 3 | Licensed Clinical Social Worker | Professional Credential |
| `MHA`   | 3 | Master of Health Administration | Academic Degree |
| `CNMT`  | 2 | Certified Nuclear Medicine Technologist | Professional Credential |
| `CRA`   | 2 | Certified Radiology Administrator | Professional Credential |
| `MSRS`  | 1 | Master of Science in Radiologic Sciences | Academic Degree |
| `RM`    | 2 | Registered Technologist (Mammography) — **ambiguous, see note** | Professional Credential |

> **Note on `RM`:** two letters and easily confused with initials; it appeared in
> ASRT credential strings (`"... RT RM RDMS RVT CRA"`) but should probably only be
> stripped in a post-nominal *run* (a trailing sequence of known credentials),
> not as a standalone token. Same caution applies generally to 2-letter degree
> entries already in the list (`BS`, `BA`, `DO`, `PE`).

This cluster is concentrated in one filer (American Society of Radiologic
Technologists), but the credentials are generic and will recur across health-care
nonprofits.

---

## 2. Honorific glued to the given name (no separating space)

`MR` is in `known_titles()`, but these are not split because there is no
whitespace/period between the honorific and the name:

| raw_name | first_name (got) | expected first_name |
|----------|------------------|---------------------|
| `MRWILLIAM R BLANCHARD` | `MRWILLIAM` | `WILLIAM` |
| `MRJOHN TTURNER`        | `MRJOHN`   | `JOHN`    |
| `MRHENRY WSWIFT JR`     | `MRHENRY`  | `HENRY`   |
| `MRTOM BBLACK`          | `MRTOM`    | `TOM`     |
| `MRJASON B BRANCH`      | `MRJASON`  | `JASON`   |
| `MREDWARD HUDSON`       | `MREDWARD` | `EDWARD`  |

**Suggested fix:** when a token has no leading-honorific split but *starts* with a
known personal-title prefix and the remainder is a valid given name, split it.
Guard against false splits on real names that merely begin with a short title
abbreviation — e.g. `FREDDIE` (`FR`+`EDDIE`), `FRANK`, `DREW` — by requiring the
remainder to be a census/given name **and** the full token to *not* itself be a
known name.

---

## 3. Single initial fused to the surname

A leading middle-initial (or a dropped apostrophe) is glued to the surname.
Detected by consensus — the same person appears in another year with the clean
surname:

| raw_name | last_name (got) | expected last_name |
|----------|-----------------|--------------------|
| `MR ROBERT CMARTIN JR` | `CMARTIN`   | `MARTIN`   |
| `KERRY BMACKEY JR`     | `BMACKEY`   | `MACKEY`   |
| `MRJACK BKEY III`      | `BKEY`      | `KEY`      |
| `MRHENRY WSWIFT JR`    | `WSWIFT`    | `SWIFT`    |
| `MRWILLIAM CWOOLFOLK`  | `CWOOLFOLK` | `WOOLFOLK` |
| `MR JOSEPH WSMITH JR`  | `WSMITH`    | `SMITH`    |
| `THOMAS O'WEEKS`       | `OWEEKS`    | `WEEKS`    |

These are middle initials that lost their space during upstream extraction
(`WILLIAM C WOOLFOLK` → `WILLIAM CWOOLFOLK`), so the parser reads `C`+surname as
one token. The last row is apostrophe handling (`O'WEEKS`), a separate but related
tokenization case. This one is hard to fix inside a single-record parse (a bare
`WSMITH` is not obviously wrong); flagging is easiest downstream, but a heuristic
— a surname beginning with a single consonant immediately followed by a capital
that yields a valid surname — could catch the common cases.

---

## Reproduce

```r
# synthid
flags <- flag_links(linked_slice_enriched)   # 6-way categorized link review
log   <- parse_fail_log(linked_slice_enriched,
                        path = "peopleparser_parse_fails.csv")
table(log$defect_type)
#>       credential_in_surname   honorific_glue   honorific_glue; surname_letter_glue   surname_letter_glue
#>                          17                7                                     4                     4
```

Priority is **Defect 1** (a data-only edit to the `known_titles()` tribble, high
volume, zero-risk) — Defects 2 and 3 are tokenizer logic and can follow.
