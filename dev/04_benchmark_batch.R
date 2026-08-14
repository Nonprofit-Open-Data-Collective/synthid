#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Find the batch-size knee empirically. Because within-EIN pairs grow LINEARLY
# in EIN count (blocking prevents cross-EIN pairs), per-EIN time should be flat
# across batch sizes -- this confirms that and gives you a per-EIN cost to
# extrapolate the full run.
#
# Usage:  Rscript dev/04_benchmark_batch.R
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(duckdb)
})
suppressMessages(devtools::load_all(".", quiet = TRUE))

DB_PATH <- "data/synthid.duckdb"
con <- dbConnect(duckdb(), dbdir = DB_PATH, read_only = TRUE)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

all_eins <- dbGetQuery(con, "SELECT DISTINCT ein FROM panel ORDER BY ein")$ein

bench_n <- function(n_eins) {
  take <- head(all_eins, n_eins)
  in_list <- paste(sprintf("'%s'", gsub("'", "''", take)), collapse = ",")
  df <- dbGetQuery(con, sprintf(
    "SELECT * FROM panel WHERE ein IN (%s)", in_list))
  t <- system.time(invisible(link_panel(df)))[["elapsed"]]
  data.frame(n_eins = n_eins, n_rows = nrow(df),
             secs = round(t, 2), ms_per_ein = round(1000 * t / n_eins, 1))
}

sizes <- c(100L, 500L, 1000L, 2000L, 5000L)
sizes <- sizes[sizes <= length(all_eins)]
res <- do.call(rbind, lapply(sizes, bench_n))
print(res, row.names = FALSE)

# Extrapolate a full single-core run from the largest sample.
per_ein <- tail(res$ms_per_ein, 1) / 1000
total   <- length(all_eins)
message(sprintf("\n~%.3f s/EIN  |  %d EINs total", per_ein, total))
message(sprintf("single-core: ~%.1f h   |   on W workers: divide by W",
                per_ein * total / 3600))
