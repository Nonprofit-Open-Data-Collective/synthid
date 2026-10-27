# 06a-unit-extract.R
# Pull geography units out of the national cross-org run, in the form the CSA
# check scripts (06i, 06k, 06l, 06m) read -- so they can run on any unit, not
# only the CSA 450 test. Several units at once share one pass over the 98
# profile files.
#
# Unit ids follow 07-xorg-csa.R: a CSA code ("408"), a CBSA outside any CSA
# ("cbsa10100"), or a state's organizations outside any CBSA ("stTX").
#
# Writes, per unit, SYNTHID_DIR/partvii-v3_1/blocks/csa-test/csa=<unit>/
#   profiles.rds    the unit's profiles (all columns), with surname weights
#   pairs.parquet   its scored pairs from xorg-csa/pairs.parquet (scores >= 3),
#                   columns as 06b writes them (emp_a, emp_b, score, csa = TRUE,
#                   same_cbsa, same_ntee)
# and creates the review/ folder and dev/partvii-panel/csa-test-<unit>/.
#
#   Rscript dev/partvii-panel/06a-unit-extract.R <unit[,unit,...]> [workers=12]

source("dev/partvii-panel/_panel.R")
build_step("06a-unit-extract")
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) >= 1)
UNITS <- strsplit(args[1], ",")[[1]]
NW <- if (length(args) >= 2) as.integer(args[2]) else 12L
uid <- function(u) if (grepl("^[0-9]+$", u)) paste0("csa", u) else u     # as in 07's unit column
udir <- function(u) file.path(pv_root, "blocks", "csa-test", paste0("csa=", u))
for (u in UNITS) for (dd in c(udir(u), file.path(udir(u), "review"), file.path(pv_repo, paste0("csa-test-", u))))
  dir.create(dd, recursive = TRUE, showWarnings = FALSE)
X <- file.path(pv_root, "xorg-csa")
map <- readRDS(file.path(X, "prep", "prep-map.rds"))
sw <- readRDS(file.path(X, "prep", "surname-weight.rds"))
fmt <- function(v) trimws(format(v, big.mark = ","))

# ---- profiles: one pass over the profile files for every unit that needs them ---------------
need <- UNITS[!file.exists(file.path(udir(UNITS), "profiles.rds"))]
if (length(need)) {
  tmp <- file.path(pv_root, "blocks", "csa-test", "_extract-tmp")
  dir.create(tmp, showWarnings = FALSE)
  files <- Sys.glob(file.path(pv_root, "profiles", "bucket=*", "part-0.parquet"))
  stamp("profiles: one pass over %d files for %d unit(s): %s", length(files), length(need), paste(need, collapse = ", "))
  cl <- parallel::makePSOCKcluster(NW)
  t0 <- Sys.time()
  invisible(parallel::parLapply(cl, files, function(f, need, ids, csa_of, tmp) {
    p <- as.data.frame(arrow::read_parquet(f))
    units <- lapply(seq_len(nrow(p)), function(i) {
      cb <- p$cbsa[[i]]; cb <- cb[!is.na(cb) & nzchar(cb)]
      if (length(cb)) unique(ifelse(cb %in% names(csa_of), paste0("csa", csa_of[cb]), paste0("cbsa", cb))) else {
        st <- p$states[[i]]; st <- st[!is.na(st) & nzchar(st)]; if (length(st)) paste0("st", unique(st)) else character(0) }
    })
    b <- basename(dirname(f))
    for (k in seq_along(need)) {
      keep <- vapply(units, function(x) ids[k] %in% x, logical(1))
      if (!any(keep)) next
      q <- p[keep, , drop = FALSE]
      for (cn in names(q)) if (is.list(q[[cn]])) q[[cn]] <- lapply(q[[cn]], as.vector)   # arrow list_of -> plain lists
      dir.create(file.path(tmp, need[k]), showWarnings = FALSE)
      saveRDS(q, file.path(tmp, need[k], paste0(b, ".rds")))
    }
    NULL
  }, need = need, ids = vapply(need, uid, ""), csa_of = map$csa_of, tmp = tmp))
  parallel::stopCluster(cl)
  for (u in need) {
    parts <- Sys.glob(file.path(tmp, u, "*.rds"))
    prof <- if (length(parts)) do.call(rbind, lapply(parts, readRDS)) else data.frame()
    saveRDS(list(prof = prof, sw = sw, N = NA), file.path(udir(u), "profiles.rds"))
    unlink(file.path(tmp, u), recursive = TRUE)
    stamp("%s: %s profiles", u, fmt(nrow(prof)))
  }
  stamp("profiles for %d unit(s) in %.1f min", length(need), as.numeric(Sys.time() - t0, units = "mins"))
} else stamp("profiles already extracted for %s", paste(UNITS, collapse = ", "))

# ---- pairs: one read of the national pairs, split by unit -----------------------------------
need <- UNITS[!file.exists(file.path(udir(UNITS), "pairs.parquet"))]
if (length(need)) {
  src <- file.path(X, "pairs.parquet")
  if (!file.exists(src)) stop("national pairs not ready: ", src, " (07-xorg-csa.R has not finished)")
  ids <- vapply(need, uid, "")
  pr <- as.data.table(read_parquet(src, col_select = c("emp_a", "emp_b", "score", "unit", "same_cbsa", "same_ntee")))[unit %in% ids]
  for (k in seq_along(need)) {
    q <- pr[unit == ids[k]][, `:=`(csa = TRUE, unit = NULL)]
    write_parquet_atomic(as.data.frame(q), file.path(udir(need[k]), "pairs.parquet"))
    stamp("%s: %s scored pairs (>= 3), %s at >= 7", need[k], fmt(nrow(q)), fmt(sum(q$score >= 7)))
  }
} else stamp("pairs already extracted for %s", paste(UNITS, collapse = ", "))
