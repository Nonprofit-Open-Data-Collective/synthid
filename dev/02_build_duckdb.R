#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Stage C: stack every preprocessed year into one persistent DuckDB file and
# assign each EIN to a batch.
#
# The batch assignment tiles the DISTINCT eins, so every EIN lands wholly in
# one batch -- required, because synthid's blocking, cluster closure, and the
# one-record-per-org-year invariant are all within-EIN. Splitting an EIN
# across batches would silently corrupt the linkage.
#
# There is NO global state to precompute: the surname-rarity weight is computed
# per-EIN inside synthid (score.R), so batches are fully independent.
#
# Usage:
#   Rscript dev/02_build_duckdb.R [eins_per_batch]
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(duckdb)
})

args <- commandArgs(trailingOnly = TRUE)
EINS_PER_BATCH <- if (length(args) >= 1) as.integer(args[[1]]) else 5000L

PREPROC_GLOB <- "data/preproc/part7_*.parquet"
DB_PATH      <- "data/synthid.duckdb"

if (file.exists(DB_PATH)) file.remove(DB_PATH)   # rebuild from scratch
con <- dbConnect(duckdb(), dbdir = DB_PATH)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

# Give DuckDB some room; tune to your machine.
dbExecute(con, "PRAGMA threads=4")
dbExecute(con, "PRAGMA memory_limit='8GB'")

# --- Stack all years into one physical table -------------------------------
dbExecute(con, sprintf(
  "CREATE TABLE panel AS SELECT * FROM read_parquet('%s')", PREPROC_GLOB))

n_rows <- dbGetQuery(con, "SELECT COUNT(*) n FROM panel")$n
n_eins <- dbGetQuery(con, "SELECT COUNT(DISTINCT ein) n FROM panel")$n
n_yrs  <- dbGetQuery(con, "SELECT COUNT(DISTINCT taxyr) n FROM panel")$n

# --- Assign each DISTINCT ein to a batch (whole-EIN guarantee) --------------
n_tiles <- max(1L, ceiling(n_eins / EINS_PER_BATCH))
dbExecute(con, sprintf("
  CREATE TABLE ein_batches AS
  SELECT ein, ntile(%d) OVER (ORDER BY ein) AS batch_id
  FROM (SELECT DISTINCT ein FROM panel)", n_tiles))

# Index so per-batch pulls are cheap.
dbExecute(con, "CREATE INDEX idx_panel_ein   ON panel(ein)")
dbExecute(con, "CREATE INDEX idx_batches_bid ON ein_batches(batch_id)")

sizes <- dbGetQuery(con, "
  SELECT batch_id, COUNT(*) n_eins
  FROM ein_batches GROUP BY batch_id ORDER BY batch_id")

message(sprintf("panel: %d rows, %d EINs, %d years", n_rows, n_eins, n_yrs))
message(sprintf("batches: %d (target %d EINs/batch; actual %d-%d EINs)",
                n_tiles, EINS_PER_BATCH, min(sizes$n_eins), max(sizes$n_eins)))
message(sprintf("wrote %s", DB_PATH))
