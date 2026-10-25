# 01-classify.R
# Run titleclassifier (steps 01-09) on every Part VII Section A row of the
# efile v3.1 release, TY2010-2024, one tax year at a time.
#
# Unit: the filing chosen in dev/devdata/02-frame.R, the latest submission per
# EIN2 x TAX_YEAR, so an amended return replaces the original.
#
# Writes SYNTHID_DIR/partvii-v3_1/classified/classified-<year>.parquet: every
# person-title row (a person with a split title has one row per title), all
# source columns plus the classification fields. A year already written is
# skipped, so the script can be stopped and re-run. A year that fails is logged
# and the run moves on to the next year.
#
#   Rscript dev/partvii-panel/01-classify.R [workers] [years, e.g. 2010:2012]

source("dev/partvii-panel/_panel.R")
suppressPackageStartupMessages(library(titleclassifier))
args  <- commandArgs(trailingOnly = TRUE)
NW    <- if (length(args) >= 1) as.integer(args[1]) else max(1L, parallel::detectCores(logical = FALSE) - 2L)
yrs   <- if (length(args) >= 2) eval(parse(text = args[2])) else YEARS
CHUNK_FILINGS <- 500L   # filings per classify_titles() call

info <- pkg_info()
fwrite(info, file.path(pv_root, "build-info-classify.csv"))
print(info)

cl <- parallel::makePSOCKcluster(NW)
on.exit(parallel::stopCluster(cl), add = TRUE)
invisible(parallel::clusterEvalQ(cl, suppressPackageStartupMessages(library(titleclassifier))))

frame <- readRDS(file.path(sd_frame, "filings.rds"))
setDT(frame)
# NTEE code and 501(c) subsection from the unified BMF (dev/devdata/04-extract.R
# cache), for step 09's organization type (titleclassifier F-033). Without them
# the type is inferred from the name and titles only.
bmf <- as.data.table(read_parquet(Sys.glob(file.path(sd_root, "bmf", "bmf", "*.parquet")),
                                  col_select = c("EIN2", "ntee_code_clean", "subsection_code")))
stopifnot(!anyDuplicated(bmf$EIN2))

for (y in yrs) {
  f <- cls_file(y)
  if (file.exists(f)) { stamp("%d: done already", y); next }
  t0  <- Sys.time()
  raw <- as.data.table(read_parquet(eff_path("F9-P07-T01-COMPENSATION", y)))
  n0  <- nrow(raw)
  raw <- raw[OBJECTID %in% frame[TAX_YEAR == y, OBJECTID]]
  raw[, TAX_YEAR := as.character(as.integer(TAX_YEAR))]
  raw <- merge(raw, bmf, by = "EIN2", all.x = TRUE, sort = FALSE)
  raw <- as.data.frame(raw)
  raw[] <- lapply(raw, function(v) { v <- as.character(v); v[is.na(v)] <- ""; v })
  objs <- unique(raw$OBJECTID)
  nf   <- length(objs)
  stamp("%d: %s Part VII rows (%s dropped as superseded filings), %s filings", y,
        format(nrow(raw), big.mark = ","), format(n0 - nrow(raw), big.mark = ","), format(nf, big.mark = ","))
  # classify_titles()'s own parallel path serializes its whole input to every
  # worker, so it slows as data x workers. Instead each worker here gets
  # fixed chunks of CHUNK_FILINGS filings (in file order) and runs
  # classify_titles() in-process (workers = 1 is sequential). Since
  # titleclassifier bdfda4e (PR #35) the result does not depend on the chunking.
  chunks <- split(raw, ceiling(match(raw$OBJECTID, objs) / CHUNK_FILINGS))
  rm(raw); gc(verbose = FALSE)
  out <- tryCatch(parallel::parLapplyLB(cl, chunks, function(x) data.table::as.data.table(suppressMessages(
           titleclassifier::classify_titles(x, batch_size = .Machine$integer.max, workers = 1, preserve_input = TRUE)))),
         error = function(e) { stamp("%d: FAILED: %s", y, conditionMessage(e)); NULL })
  rm(chunks); gc(verbose = FALSE)
  if (is.null(out)) next
  out <- rbindlist(out, use.names = TRUE, fill = TRUE)
  write_parquet(out, f)
  stamp("%d: -> %s person-title rows in %.1f min", y, format(nrow(out), big.mark = ","),
        as.numeric(Sys.time() - t0, units = "mins"))
  rm(out); gc(verbose = FALSE)
}
stamp("years written: %s", paste(YEARS[file.exists(cls_file(YEARS))], collapse = " "))
