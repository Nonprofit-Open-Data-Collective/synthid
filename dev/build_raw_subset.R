#!/usr/bin/env Rscript
# Cache the raw Part VII rows for a sample of full-panel-consistent EINs across
# all 15 years, so runtime profiling can subsample without re-reading 18GB.
suppressPackageStartupMessages({library(data.table)})

RAW_DIR <- "dev/data"; TABLE <- "F9-P07-T01-COMPENSATION"; years <- 2010:2024
N <- 1000
info <- readRDS("dev/data/panel_eins_2010_2024.rds")
set.seed(42)
samp <- sample(info$consistent, N)
cat(sprintf("Sampled %d of %d consistent EINs (seed 42)\n", length(samp), length(info$consistent)))

parts <- vector("list", length(years))
for (i in seq_along(years)) {
  y <- years[i]
  f <- file.path(RAW_DIR, sprintf("%s-%d.CSV", TABLE, y))
  t0 <- Sys.time()
  d  <- fread(f, colClasses = list(character = "EIN2"), showProgress = FALSE)
  d  <- d[EIN2 %in% samp]
  if (!"TAX_YEAR" %in% names(d)) d[, TAX_YEAR := y]
  parts[[i]] <- d
  cat(sprintf("[%d] kept %d rows for sampled EINs  (%.1fs)\n",
              y, nrow(d), as.numeric(Sys.time() - t0, units = "secs")))
}
raw <- rbindlist(parts, use.names = TRUE, fill = TRUE)
cat(sprintf("\nTotal raw rows across 15 years for %d EINs: %d\n", N, nrow(raw)))
saveRDS(list(raw = as.data.frame(raw), eins = samp),
        "dev/data/raw_subset_1000_15yr.rds")
cat("Saved -> dev/data/raw_subset_1000_15yr.rds\n")
