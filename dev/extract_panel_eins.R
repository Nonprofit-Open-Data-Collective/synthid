#!/usr/bin/env Rscript
# Find EINs present in ALL years of the 2010-2024 Part VII panel, then sample a
# profiling set. Reads only the EIN2 column from each ~1GB CSV (fast).
suppressPackageStartupMessages({library(data.table)})

RAW_DIR <- "dev/data"
TABLE   <- "F9-P07-T01-COMPENSATION"
years   <- 2010:2024
out_rds <- "dev/data/panel_eins_2010_2024.rds"

per_year <- list()
for (y in years) {
  f <- file.path(RAW_DIR, sprintf("%s-%d.CSV", TABLE, y))
  t0 <- Sys.time()
  d  <- fread(f, select = "EIN2", colClasses = list(character = "EIN2"),
              showProgress = FALSE)
  u  <- unique(d$EIN2)
  u  <- u[!is.na(u) & nzchar(u)]
  per_year[[as.character(y)]] <- u
  cat(sprintf("[%d] %d rows, %d unique EINs  (%.1fs)\n",
              y, nrow(d), length(u), as.numeric(Sys.time() - t0, units = "secs")))
}

# EINs present in every year = intersection across all 15 sets
consistent <- Reduce(intersect, per_year)
cat(sprintf("\nEINs present in ALL %d years: %d\n", length(years), length(consistent)))

# also report coverage distribution (in how many years each EIN appears)
allein <- unlist(per_year, use.names = FALSE)
tab    <- table(table(allein))
cat("Distribution of #years-present across all EINs:\n")
print(tab)

saveRDS(list(per_year = per_year, consistent = consistent, years = years), out_rds)
cat(sprintf("\nSaved -> %s\n", out_rds))
