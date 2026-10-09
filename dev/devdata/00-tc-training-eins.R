# 00-tc-training-eins.R
# Rebuild the list of organizations whose people trained titleclassifier's
# step 09 role model, so the development dataset can leave them out (D-003).
#
# The model (titleclassifier FINDINGS F-028, F-029) was trained on:
#   - every full-990 person in the 2019-2023 calibration sample
#     (titleclassifier data-raw/partvii-validation/12-panel-sample.R), and
#   - the 990-EZ people of the form-switching organizations sampled in
#     18-switch-labels.R.
# Neither EIN list is in the titleclassifier repository; both draws are
# deterministic (seed 2026) from the efile v2.3 Part I and Part VII tables.
#
# This script downloads those tables (TY2019-2023) into
# SYNTHID_DIR/efile_v2_3, runs titleclassifier's own 12-panel-sample.R
# unchanged, and confirms the result reproduces the committed
# panel-sample-cells.csv byte for byte (git diff is empty). The switcher draw
# repeats 18-switch-labels.R step 1.
#
# Writes SYNTHID_DIR/frame/tc-training-eins.rds (EIN2, source).
#
#   Rscript dev/devdata/00-tc-training-eins.R

source("dev/devdata/_config.R")

tc_repo <- normalizePath(Sys.getenv("TITLECLASSIFIER_REPO", "../titleclassifier"), winslash = "/")
v23     <- file.path(sd_root, "efile_v2_3")
dir.create(file.path(v23, "efile"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(v23, "sample"), recursive = TRUE, showWarnings = FALSE)
years23 <- 2019:2023

for (t in c("F9-P01-T00-SUMMARY", "F9-P07-T01-COMPENSATION")) for (y in years23) {
  f <- file.path(v23, "efile", sprintf("%s-%d.parquet", t, y))
  if (file.exists(f)) next
  u <- sprintf("https://nccs-efile.s3.us-east-1.amazonaws.com/public/efile_v2_3/%s-%d.parquet", t, y)
  options(timeout = 3600)
  download.file(u, f, mode = "wb", quiet = TRUE)
  stamp("downloaded %s", basename(f))
}

# ---- calibration sample: run titleclassifier's script as is -------------------
smp_file <- file.path(v23, "sample", "panel-sample-2019-2023.rds")
if (!file.exists(smp_file)) {
  owd <- setwd(tc_repo)
  Sys.setenv(PARTVII_DIR = v23)
  sys.source("data-raw/partvii-validation/12-panel-sample.R", envir = new.env())
  diff <- system2("git", c("diff", "--stat", "--", "data-raw/partvii-validation/panel-sample-cells.csv"),
                  stdout = TRUE)
  setwd(owd)
  if (length(diff)) stop("panel-sample-cells.csv differs from the committed copy: the draw ",
                         "did not reproduce. Restore it with git checkout in titleclassifier.")
  stamp("calibration sample reproduced: panel-sample-cells.csv identical to the committed file")
}
cal <- unique(readRDS(smp_file)$EIN2)

# ---- switchers: 18-switch-labels.R step 1 -------------------------------------
fr <- rbindlist(lapply(years23, function(y) as.data.table(read_parquet(
  file.path(v23, "efile", sprintf("F9-P01-T00-SUMMARY-%d.parquet", y)),
  col_select = c("EIN2", "OBJECTID", "RETURN_TYPE", "RETURN_TIME_STAMP", "F9_01_EXP_TOT_CY")))[, year := y]))
setorder(fr, EIN2, year, -RETURN_TIME_STAMP); fr <- fr[!duplicated(fr[, .(EIN2, year)])]
pr <- merge(fr[, .(EIN2, year, f0 = RETURN_TYPE, o0 = OBJECTID)],
            fr[, .(EIN2, year = year - 1L, f1 = RETURN_TYPE, o1 = OBJECTID)], by = c("EIN2", "year"))
pr <- pr[f0 != f1 & !EIN2 %in% cal]
set.seed(2026)
up   <- pr[f0 == "990EZ"][, .SD[sample(.N, 1)], by = EIN2][sample(.N, min(.N, 3000))]
down <- pr[f0 == "990" & !EIN2 %in% up$EIN2][, .SD[sample(.N, 1)], by = EIN2][sample(.N, min(.N, 1500))]
sw <- unique(c(up$EIN2, down$EIN2))

out <- rbind(data.table(EIN2 = cal, source = "calibration_sample"),
             data.table(EIN2 = setdiff(sw, cal), source = "switcher"))
saveRDS(out, file.path(sd_frame, "tc-training-eins.rds"))
stamp("%d calibration-sample orgs + %d switcher orgs", length(cal), length(setdiff(sw, cal)))
