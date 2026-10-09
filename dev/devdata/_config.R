# Shared settings for the development-dataset build (dev/devdata/).
#
# Data lives outside the repository in SYNTHID_DIR (default ~/Documents/SYNTHID).
# Scripts, the process description (README.md) and the findings log
# (FINDINGS.md) live here, in the repository. Run every script from the
# package root:
#
#   Rscript dev/devdata/01-download.R

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})

sd_root  <- normalizePath(Sys.getenv("SYNTHID_DIR",
                file.path(Sys.getenv("USERPROFILE", "~"), "Documents", "SYNTHID")),
                winslash = "/", mustWork = FALSE)
sd_efile <- file.path(sd_root, "efile")     # panel990 download cache (parquet)
sd_frame <- file.path(sd_root, "frame")     # population frame + cohort draw
sd_data  <- file.path(sd_root, "devdata")   # the development dataset itself
sd_logs  <- file.path(sd_root, "logs")
sd_repo  <- "dev/devdata"                   # small outputs that are committed

for (d in c(sd_efile, sd_frame, sd_data, sd_logs))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

EFILE_VERSION <- "v3_1"
YEARS         <- 2010:2024   # 2009 and 2025 are partial years in v3.1
SEED          <- 2026

# Every table the dataset draws on. Header and Part VII define the frame; the
# rest are pulled only for the sampled organizations.
TABLES <- c(
  "F9-P00-T00-HEADER",                # filer, form type, tax period, amended flags
  "F9-P01-T00-SUMMARY",               # 990 Part I: expenses, employees, voting members
  "F9-P01-T00-SUMMARY-EZ",            # 990-EZ Part I
  "F9-P06-T00-GOVERNANCE",            # 990 Part VI: board size, independence
  "F9-P06-T00-GOVERNANCE-EZ",
  "F9-P07-T00-DIR-TRUST-KEY",         # Part VII counts (people over $100k, ...)
  "F9-P07-T01-COMPENSATION",          # Part VII Section A: one row per person
  "SJ-P02-T01-COMPENSATION-DTK"       # Schedule J Part II: pay detail per person
)

# Local path of one cached table-year, from the manifest 01-download.R writes
# (panel990 decides the cache layout).
eff_path <- local({
  man <- NULL
  function(table, year) {
    if (is.null(man)) man <<- as.data.table(readRDS(file.path(sd_efile, "download-manifest.rds")))
    p <- man[TABLE == table & YEAR == year & STATUS != "failed", PATH]
    if (length(p) != 1L) stop("no cached file for ", table, " ", year)
    p
  }
})

num <- function(x) suppressWarnings(as.numeric(x))

stamp <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", sprintf(...), "\n")
