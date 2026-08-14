#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Stage D: run synthid linkage over the batches, in parallel ACROSS batches.
#
# Parallelism model (see the analysis in the design notes):
#   * The independent unit of work is one EIN; batches are disjoint sets of
#     whole EINs, so there is zero cross-batch communication.
#   * We parallelize ACROSS batches with future/furrr and keep reclin2
#     SINGLE-THREADED inside each worker. For 375-row-ish orgs, reclin2's own
#     socket parallelism costs more than it saves -- the win is core-per-batch.
#   * Windows => multisession (no forking).
#
# Each worker opens its OWN read-only DuckDB connection (concurrent readers are
# fine), links its batch, and writes one Parquet part file. Nothing flows back
# through the main process except a small per-batch summary.
#
# Usage:
#   Rscript dev/03_run_batched.R [n_workers]
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(future)
  library(furrr)
  library(duckdb)
})

args      <- commandArgs(trailingOnly = TRUE)
N_WORKERS <- if (length(args) >= 1) as.integer(args[[1]])
             else max(1L, parallel::detectCores(logical = FALSE) - 1L)

PKG_DIR <- normalizePath(".", winslash = "/")     # run from the package root
DB_PATH <- "data/synthid.duckdb"
OUT_DIR <- "data/linked"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(DB_PATH))

# --- Enumerate batches (tiny query in the main process) --------------------
con0 <- dbConnect(duckdb(), dbdir = DB_PATH, read_only = TRUE)
batch_ids <- dbGetQuery(con0, "SELECT DISTINCT batch_id FROM ein_batches ORDER BY batch_id")$batch_id
dbDisconnect(con0, shutdown = TRUE)
message(sprintf("linking %d batches on %d workers", length(batch_ids), N_WORKERS))

plan(multisession, workers = N_WORKERS)

# --- Per-batch worker ------------------------------------------------------
link_one_batch <- function(bid) {
  # Keep every numeric library single-threaded so we don't oversubscribe cores.
  Sys.setenv(OMP_NUM_THREADS = "1")
  try(data.table::setDTthreads(1L), silent = TRUE)
  suppressMessages(devtools::load_all(PKG_DIR, quiet = TRUE))

  # Own read-only connection to the shared DB (concurrent readers are safe).
  con <- dbConnect(duckdb(), dbdir = DB_PATH, read_only = TRUE)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  df <- dbGetQuery(con, sprintf("
    SELECT p.*
    FROM panel p JOIN ein_batches b USING (ein)
    WHERE b.batch_id = %d
    ORDER BY p.ein", bid))

  # reclin2 stays single-threaded: we simply never pass it a cluster.
  linked <- synthid::link_panel(df)

  # Write this batch's result as one Parquet part via a throwaway in-mem DB.
  out_path <- file.path(OUT_DIR, sprintf("part-%05d.parquet", bid))
  w <- dbConnect(duckdb())
  duckdb_register(w, "linked", linked)
  dbExecute(w, sprintf(
    "COPY (SELECT * FROM linked) TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)",
    gsub("'", "''", normalizePath(out_path, winslash = "/", mustWork = FALSE))))
  dbDisconnect(w, shutdown = TRUE)

  r <- attr(linked, "synthid")
  data.frame(batch_id = bid, n_records = r$n_records,
             n_orgs = r$n_orgs, n_persons = r$n_clusters,
             collisions_blocked = r$rejected_edges)
}

# --- Fan out ---------------------------------------------------------------
t0 <- Sys.time()
summ <- future_map_dfr(
  batch_ids, link_one_batch,
  .options = furrr_options(seed = TRUE,
                           globals = c("PKG_DIR", "DB_PATH", "OUT_DIR"),
                           packages = c("duckdb", "devtools"))
)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))

message(sprintf("done: %d records -> %d persons across %d orgs in %.1f min",
                sum(summ$n_records), sum(summ$n_persons),
                sum(summ$n_orgs), elapsed))
message(sprintf("collisions blocked: %d", sum(summ$collisions_blocked)))
message(sprintf("results: %s/part-*.parquet", OUT_DIR))
# Query the full linked panel later with, e.g.:
#   SELECT * FROM read_parquet('data/linked/part-*.parquet')
