# 01-download.R
# Download the efile v3.1 parquet tables the development dataset draws on,
# TY2010-2024, into SYNTHID_DIR/efile with panel990::download_tables().
# Files already in the cache are reused, so the script can be re-run.
#
# Writes SYNTHID_DIR/efile/download-manifest.rds (one row per table-year with
# the local path, status and size), which later scripts use to find files.
#
#   Rscript dev/devdata/01-download.R

source("dev/devdata/_config.R")
library(panel990)
stopifnot(efile_version() == EFILE_VERSION)

dl <- download_tables(years = YEARS, tables = TABLES, path = sd_efile,
                      cache = "retain", retry_max = 5, timeout = 3600)
man <- as.data.table(dl$manifest)
setnames(man, toupper(names(man)))
print(man[, .N, by = STATUS])

# normalise the columns later scripts rely on
if (!"TABLE" %in% names(man)) setnames(man, grep("TABLE", names(man), value = TRUE)[1], "TABLE")
if (!"YEAR"  %in% names(man)) setnames(man, grep("YEAR",  names(man), value = TRUE)[1], "YEAR")
man[, BYTES := file.size(PATH)]
saveRDS(man, file.path(sd_efile, "download-manifest.rds"))

stamp("%d files, %.2f GB, %d failed", nrow(man), sum(man$BYTES, na.rm = TRUE) / 1e9,
      sum(man$STATUS == "failed"))
