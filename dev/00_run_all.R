#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# End-to-end driver: preprocess every year (Stage A/B), build the DuckDB with
# EIN batches (Stage C), then run the batched linkage (Stage D).
#
# Each stage runs as its OWN Rscript subprocess -- a fresh session per year so
# titleclassifier/peopleparser's internal future::plan() never bleeds between
# runs, and so a crash in one year can't take down the whole driver's state.
# Years whose Parquet already exists are SKIPPED, so re-running resumes.
#
# Usage:
#   Rscript dev/00_run_all.R                       # 2007:2021, defaults
#   Rscript dev/00_run_all.R 2019:2021             # a year range
#   Rscript dev/00_run_all.R 2019,2020,2021 5000 8 # explicit years, batch, workers
#     args: [1] years  [2] eins_per_batch  [3] workers
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

parse_years <- function(spec) {
  if (grepl(":", spec, fixed = TRUE)) {
    ab <- as.integer(strsplit(spec, ":", fixed = TRUE)[[1]])
    seq(ab[1], ab[2])
  } else {
    as.integer(strsplit(spec, ",", fixed = TRUE)[[1]])
  }
}

years          <- if (length(args) >= 1) parse_years(args[[1]]) else 2007:2021
EINS_PER_BATCH <- if (length(args) >= 2) args[[2]] else "5000"
WORKERS        <- if (length(args) >= 3) args[[3]] else NULL

RSCRIPT <- file.path(R.home("bin"),
                     if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
DEV     <- "dev"                                 # run from the package root
PREPROC <- "data/preproc"

run <- function(script, script_args = character()) {
  cmd  <- file.path(DEV, script)
  code <- system2(RSCRIPT, c(shQuote(cmd), script_args), stdout = "", stderr = "")
  if (!identical(code, 0L))
    stop(sprintf("STOP: %s exited with status %s", script, code), call. = FALSE)
}

banner <- function(x) message("\n==================== ", x, " ====================")

# --- Stage A/B: preprocess each year (skip finished years) -----------------
banner(sprintf("PREPROCESS %d year(s): %s", length(years),
               paste(range(years), collapse = "-")))
for (y in years) {
  out <- file.path(PREPROC, sprintf("part7_%d.parquet", y))
  if (file.exists(out)) { message(sprintf("[%d] skip (exists)", y)); next }
  message(sprintf("[%d] preprocessing...", y))
  run("01_preprocess_year.R", as.character(y))
}

# --- Stage C: stack + assign EIN batches -----------------------------------
banner("BUILD DUCKDB + EIN BATCHES")
run("02_build_duckdb.R", EINS_PER_BATCH)

# --- Stage D: batched linkage ----------------------------------------------
banner("RUN BATCHED LINKAGE")
run("03_run_batched.R", if (is.null(WORKERS)) character() else as.character(WORKERS))

banner("DONE")
message("Linked panel: data/linked/part-*.parquet")
message("Query it, e.g.:  SELECT * FROM read_parquet('data/linked/part-*.parquet')")
