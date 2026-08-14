#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Benchmark the memoized-comparator speedup on full 15-year EIN panels.
# Builds (and caches) a preprocessed df for N EINs, then links it twice --
# baseline vs options(synthid.memoize_comparators = TRUE) -- and asserts the
# EMP_ID assignment is byte-identical before reporting the speedup.
#
# Usage: Rscript dev/bench_memoize.R [N_EINS]
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr); library(data.table); library(titleclassifier)
  library(peopleparser); library(googlesheets4)
})
devtools::load_all(".", quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
N    <- if (length(args) >= 1) as.integer(args[[1]]) else 100L

timeit <- function(expr) { t0 <- Sys.time(); v <- force(expr)
  list(v = v, s = as.numeric(Sys.time() - t0, units = "secs")) }

# --- input df: reconstruct from the saved 100-EIN linked output ------------
# link_panel() returns the input df with EMP_ID/EMP_N_* appended, so dropping
# those three columns recovers the exact scorer input -- no need to re-run the
# (network-bound, furrr-parallel) classify_titles/parse_names preprocessing.
linked_src <- readRDS("dev/data/linked_full_panel_100eins.rds")
df <- linked_src[, setdiff(names(linked_src), c("EMP_ID","EMP_N_RECORDS","EMP_N_YEARS"))]
keep_eins <- head(unique(df$ein), N)
df <- df[df$ein %in% keep_eins, , drop = FALSE]

cat(sprintf("\n=== MEMOIZE BENCHMARK: %d EINs, %d person-years, years %d-%d ===\n",
            length(unique(df$ein)), nrow(df), min(df$taxyr), max(df$taxyr)))

# --- unit equivalence: wrapper output must equal the raw comparator ---------
set.seed(1)
xa <- sample(c("SMITH","MC-LANE","VAN DER BERG","COHEN-GANTSOUDES", NA), 50, TRUE)
ya <- sample(c("SMYTHE","MCLANE","VANDERBERG","COHEN", NA), 50, TRUE)
raw_out  <- synthid:::compare_last_names(xa, ya)
memo_fn  <- synthid:::memoize_comparator(synthid:::compare_last_names)
memo_out <- memo_fn(xa, ya)                    # first pass (all misses)
memo_out2<- memo_fn(xa, ya)                    # second pass (all cache hits)
stopifnot(identical(raw_out, memo_out), identical(raw_out, memo_out2))
cat("[unit] memoized comparator == raw comparator (misses and hits): OK\n")

# --- baseline vs memoized ---------------------------------------------------
options(synthid.memoize_comparators = FALSE)
base <- timeit(synthid::link_panel(df))
cat(sprintf("[base]     link_panel : %8.1fs\n", base$s))

options(synthid.memoize_comparators = TRUE)
memo <- timeit(synthid::link_panel(df))
cat(sprintf("[memoized] link_panel : %8.1fs\n", memo$s))
options(synthid.memoize_comparators = FALSE)

# --- correctness gate: identical EMP_ID assignment --------------------------
ok <- identical(base$v$EMP_ID, memo$v$EMP_ID)
cat(sprintf("\n[gate] identical EMP_ID vectors: %s\n", ok))
if (!ok) {
  d <- sum(base$v$EMP_ID != memo$v$EMP_ID)
  cat(sprintf("       MISMATCH in %d of %d rows -- NOT behavior-preserving!\n", d, nrow(df)))
} else {
  np <- length(unique(base$v$EMP_ID))
  cat(sprintf("       persons: %d  (both runs)\n", np))
  cat(sprintf("\n=== SPEEDUP: %.2fx  (%.1fs -> %.1fs, %.1f%% faster) ===\n",
              base$s / memo$s, base$s, memo$s, 100 * (1 - memo$s / base$s)))
  cat(sprintf("Per-EIN link: base=%.1f ms  memoized=%.1f ms\n",
              1000*base$s/length(unique(df$ein)), 1000*memo$s/length(unique(df$ein))))
  cat(sprintf("Extrapolated single-thread over 24,875 consistent EINs: %.1f h -> %.1f h\n",
              base$s/length(unique(df$ein))*24875/3600, memo$s/length(unique(df$ein))*24875/3600))
}
