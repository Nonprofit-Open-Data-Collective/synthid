#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Stage A/B: download one year of Form 990 Part VII compensation from the NCCS
# S3 bucket, standardize titles, parse names, and write one Parquet file whose
# columns are exactly what synthid::link_panel() consumes.
#
# Run ONCE per tax year. Output is a rebuildable cache -- never run inside the
# linkage loop.
#
# Pipeline (all real APIs, verified end-to-end on 2010-2012 data):
#   local CSV (dev/data) via fread, else panel990 download  -> raw Part VII (F9_07_COMP_DTK_*)
#   titleclassifier::classify_titles(preserve_input = TRUE)  -> ein, taxyr, dtk.name, title.standard, ...
#   << collapse to one row per person-year >>                -> respects synthid's one-record-per-org-year invariant
#   peopleparser::parse_names(dtk.name)                      -> first/middle/last/suffix/salutation/gender
#
# titleclassifier and peopleparser each parallelize internally (furrr multisession),
# so a single year already uses your cores. This stage is independent of synthid's
# own EIN-batch parallelism in dev/03.
#
# Usage:  Rscript dev/01_preprocess_year.R 2019
#   Reads dev/data/F9-P07-T01-COMPENSATION-<year>.CSV if present; otherwise
#   downloads the year from the NCCS S3 store.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(data.table)      # fread: reads the messy free-text CSVs cleanly
  library(duckdb)          # Parquet writer (no arrow dependency)
  library(panel990)
  library(titleclassifier)
  library(peopleparser)
  library(googlesheets4)
})

args <- commandArgs(trailingOnly = TRUE)
year <- if (length(args) >= 1) as.integer(args[[1]]) else stop("Pass a tax year, e.g. 2019")

PART7_TABLE <- "F9-P07-T01-COMPENSATION"   # NCCS canonical name for Part VII, Table 01
RAW_DIR     <- "dev/data"                  # local raw CSVs, if present, are used before S3
OUT_DIR     <- "data/preproc"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(OUT_DIR, sprintf("part7_%d.parquet", year))

# --- A. read Part VII for this year (local CSV if present, else S3) ---------
# NOTE: DuckDB's CSV sniffer FAILS on these files -- the free-text title/name
# fields defeat dialect detection -- so we read raw CSVs with data.table::fread,
# which is lenient and infers the numeric hours/comp types titleclassifier needs.
local_csv <- file.path(RAW_DIR, sprintf("%s-%d.CSV", PART7_TABLE, year))
if (file.exists(local_csv)) {
  message(sprintf("[%d] reading local CSV: %s", year, local_csv))
  raw <- as.data.frame(fread(local_csv, showProgress = FALSE,
                             colClasses = list(character = "EIN2")))
  if (!"TAX_YEAR" %in% names(raw)) raw$TAX_YEAR <- year
} else {
  dl  <- panel990::download_tables(years = year, tables = PART7_TABLE)
  rd  <- panel990::read_tables(dl)                 # panel990 also reads via fread
  raw <- rd$tables[[paste(PART7_TABLE, year, sep = "::")]]  # TAX_YEAR added by read_tables
}
if (is.null(raw) || !nrow(raw))
  stop(sprintf("No Part VII rows read for %d (table %s).", year, PART7_TABLE))
message(sprintf("[%d] %d raw Part VII rows", year, nrow(raw)))

# --- B1. standardize titles ------------------------------------------------
# classify_titles() runs the full titleclassifier pipeline in parallel (groups
# by OBJECTID) and pulls its crosswalk assets from Google Sheets. gs4_deauth()
# forces public-sheet access so a non-interactive run never blocks on OAuth.
# preserve_input = TRUE keeps the source columns alongside the classified ones.
# It expects the raw NCCS columns EIN2 / OBJECTID / F9_07_COMP_DTK_* to be present.
gs4_deauth()
classified <- titleclassifier::classify_titles(raw, preserve_input = TRUE)

# --- B1.5 Resolve to ONE authoritative filing per org-year -----------------
# EIN+TAXYR is NOT the filing grain: an organization can submit several returns
# for the same tax year (an original plus amendments), each a distinct OBJECTID
# carrying its own independent TID-00001..N sequence. If left in, the same person
# appears once per filing -- inflating the panel and letting superseded (stale)
# returns feed downstream data. Keep only the most recent submission per org-year
# (latest RETURN_TIME_STAMP; amended-preferred, then max OBJECTID as a
# deterministic final tie-break). Done in base R on extracted vectors so
# classify_titles()'s data.table IDate columns can't break the operation.
.oid <- as.character(classified[["OBJECTID"]])
.oy  <- paste(as.character(classified[["ein"]]), as.character(classified[["taxyr"]]))
.ts  <- as.character(classified[["RETURN_TIME_STAMP"]]); .ts[is.na(.ts)] <- ""
.am  <- toupper(trimws(as.character(classified[["RETURN_AMENDED_X"]]))) %in%
          c("TRUE", "T", "1", "X", "Y", "YES")
.first  <- !duplicated(.oid)                       # one row per filing
filings <- data.frame(OBJECTID = .oid[.first], oy = .oy[.first],
                      ts = .ts[.first], amended = .am[.first],
                      stringsAsFactors = FALSE)
# Ascending sort puts the winner last within each org-year; take the last.
filings <- filings[order(filings$oy, filings$ts, filings$amended, filings$OBJECTID), ]
winners <- filings$OBJECTID[!duplicated(filings$oy, fromLast = TRUE)]
.keep   <- .oid %in% winners
n_multi <- sum(table(filings$oy) > 1L)
message(sprintf(
  "[%d] filing-resolution: %d org-year(s) had multiple filings; dropped %d of %d rows from superseded returns",
  year, n_multi, sum(!.keep), length(.keep)))
classified <- classified[.keep, ]
rm(.oid, .oy, .ts, .am, .first, filings, winners, .keep)

# --- Collapse to ONE row per person (source Part VII line) -----------------
# split_titles() emits one row per title, so a person holding N titles appears
# N times. We collapse to the source person-line grain -- (OBJECTID, TABLE_ID) --
# keeping the PRIMARY title (title.order == 1). TABLE_ID is the stored per-person
# key within a filing and is unique there, so after filing-resolution above this
# is exactly one row per person per org-year, and -- unlike the former
# ein+taxyr+dtk.name key -- it keeps two genuinely different people who share a
# name in the same filing as distinct records (PERSON_YEAR_ID then maps 1:1 to
# panel rows, collision-free by construction).
#
# DECISION POINT (revisit with domain judgment): alternatives are keep the
# highest-comp title, or concatenate titles into one string. Change the
# slice_min() key below to switch policy.
#
# We subset to the needed columns via base R FIRST: classify_titles() carries
# data.table IDate columns that otherwise break dplyr/tibble grouping
# ("Corrupt <IDate>. Expected integer storage, not double storage").
# Carry the source key (OBJECTID/TABLE_ID) and evaluation fields (raw name, raw
# title, total hours, total comp) through the collapse so link_panel can stamp a
# PERSON_YEAR_ID back-link and downstream evaluation has the raw values. The
# collapse keeps the primary-title row, so raw title is the primary title's raw
# form; tot.hours/tot.comp are person-year invariant (verified) and OBJECTID/
# TABLE_ID ride along from the surviving row.
keep_cols <- c("ein", "taxyr", "dtk.name", "org.name", "title.standard", "title.order",
               "OBJECTID", "TABLE_ID", "F9_07_COMP_DTK_NAME_PERS",
               "title.raw", "tot.hours", "tot.comp")
keep_cols <- intersect(keep_cols, names(as.data.frame(classified)))
person_year <- as.data.frame(classified)[, keep_cols] %>%
  mutate(title.order = suppressWarnings(as.numeric(.data$title.order)),
         title.order = ifelse(is.na(.data$title.order), 1, .data$title.order)) %>%
  group_by(.data$OBJECTID, .data$TABLE_ID) %>%
  slice_min(.data$title.order, n = 1, with_ties = FALSE) %>%
  ungroup()
message(sprintf("[%d] %d classified rows -> %d person-year rows",
                year, nrow(classified), nrow(person_year)))

# --- B2. parse names -------------------------------------------------------
# parse_names() returns a `name` column (= its input) plus the components.
parsed <- peopleparser::parse_names(person_year$dtk.name)

# --- Assemble exactly synthid's input contract -----------------------------
# synthid_cols(): org_id=ein, org_name=org.name, year=taxyr, name=name,
#   features = salutation, first_name, middle_name, last_name, suffix,
#              gender, title.standard
df <- data.frame(
  # Stable per-record back-link to the source Part VII row. Computed once here at
  # ingest from the raw source key; link_panel() preserves it onto the linked
  # output next to the cross-year EMP_ID. See synthid::person_year_id().
  PERSON_YEAR_ID = synthid::person_year_id(person_year$OBJECTID, person_year$TABLE_ID),
  OBJECTID       = as.character(person_year$OBJECTID),   # raw components: audit + recovery
  TABLE_ID       = as.character(person_year$TABLE_ID),
  ein            = as.character(person_year$ein),
  taxyr          = as.integer(person_year$taxyr),
  org.name       = person_year$org.name,
  name           = parsed$name,          # = person_year$dtk.name (coalesced person/org name)
  name.raw       = person_year$F9_07_COMP_DTK_NAME_PERS,  # literal Part VII person name (blank for institutions)
  salutation     = parsed$salutation,
  first_name     = parsed$first_name,
  middle_name    = parsed$middle_name,
  last_name      = parsed$last_name,
  suffix         = parsed$suffix,
  gender         = parsed$gender,
  title.standard = person_year$title.standard,
  title.raw      = person_year$title.raw,   # evaluation: raw (primary) title
  tot.hours      = person_year$tot.hours,    # evaluation: person-year total hours
  tot.comp       = person_year$tot.comp,     # evaluation: person-year total compensation
  stringsAsFactors = FALSE
)

# --- Write Parquet via a throwaway in-memory DuckDB -------------------------
con <- dbConnect(duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
duckdb_register(con, "df", df)
dbExecute(con, sprintf(
  "COPY (SELECT * FROM df) TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)",
  gsub("'", "''", normalizePath(out_path, winslash = "/", mustWork = FALSE))))

message(sprintf("[%d] wrote %d person-year rows -> %s", year, nrow(df), out_path))
