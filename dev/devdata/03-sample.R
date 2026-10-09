# 03-sample.R
# Draw the development-dataset organizations from the frame (02-frame.R).
#
# Cohorts, by the panel990 membership class over TY2010-2024 (FINDINGS.md D-002):
#   core_dev      persistent + seamless, all 15 years       3,000 orgs
#   core_holdout  persistent + seamless, all 15 years       3,000 orgs
#   entrant       enters after 2010, files through 2024     1,500
#   exit          files from 2010, stops before 2024        1,500
#   gapped        any boundary class, 1+ interior gap year  1,500
#   transient     enters after 2010, stops before 2024      1,000
# Entrant, exit and transient are seamless; every segmented organization is
# in "gapped" (its panel_type is kept as a column).
#
# The two core cohorts are one stratified draw split in half within each
# stratum, so tuning on core_dev and checking on core_holdout compare like with
# like.
#
# Strata within each cohort: organization size (median total expenses over
# its filings, 7 bins) x form mix (990 only, 990-EZ only, switcher).
# Allocation is proportional to the square root of the stratum population,
# with a floor of 25 where the stratum is that big. weight = population / drawn.
#
# Boundary filings (D-005): exit and transient organizations last seen in
# TY2023 are dropped (possible late TY2024 filers), and entrants / exits are
# stratified on the initial- / final-return flag as well.
#
# Exclusions (D-003): organizations whose people trained titleclassifier's
# step 09 role model (00-tc-training-eins.R), so role accuracy measured here
# is out of sample. Overlap with the older
# dev/test_panels_1k EINs is flagged, not excluded.
#
# Writes SYNTHID_DIR/frame/sample-orgs.rds and dev/devdata/sample-cells.csv.
#
#   Rscript dev/devdata/03-sample.R

source("dev/devdata/_config.R")
org <- readRDS(file.path(sd_frame, "orgs.rds"))

# ---- exclusions / flags ---------------------------------------------------------
tc_eins <- readRDS(file.path(sd_frame, "tc-training-eins.rds"))$EIN2   # 00-tc-training-eins.R
org[, in_tc_sample := EIN2 %in% tc_eins]
stamp("titleclassifier role-model training orgs: %d, of which %d in the frame (excluded)",
      length(tc_eins), org[, sum(in_tc_sample)])

old_panels <- "dev/test_panels_1k/panel_eins.rds"
old_eins <- if (file.exists(old_panels)) unlist(readRDS(old_panels), use.names = FALSE) else character()
org[, in_test_panels_1k := EIN2 %in% old_eins]

# boundary flags (D-005): is the first filing an initial return, the last a final return?
fil <- readRDS(file.path(sd_frame, "filings.rds"))[, .(EIN2, TAX_YEAR, F9_00_RETURN_INITIAL_X, F9_00_RETURN_FINAL_X)]
org[fil, on = .(EIN2, panel_year_first = TAX_YEAR), first_initial := i.F9_00_RETURN_INITIAL_X %in% "X"]
org[fil, on = .(EIN2, panel_year_last = TAX_YEAR),  last_final    := i.F9_00_RETURN_FINAL_X %in% "X"]
rm(fil)

pool <- org[in_tc_sample == FALSE]

pool[, cohort := fcase(
  panel_spell == "segmented",  "gapped",
  panel_type == "persistent",  "core",
  panel_type == "entrant",     "entrant",
  panel_type == "exit",        "exit",
  panel_type == "transient",   "transient")]
# D-005: an organization last seen in TY2023 may be a late TY2024 filer, not an exit
n_late <- pool[cohort %in% c("exit", "transient") & panel_year_last == max(YEARS) - 1L, .N]
pool <- pool[!(cohort %in% c("exit", "transient") & panel_year_last == max(YEARS) - 1L)]
stamp("dropped %d exit/transient orgs last seen in %d (possible late filers)", n_late, max(YEARS) - 1L)

# D-005: entrants and exits are stratified on whether the boundary filing is
# flagged as an initial / final return, so true births and deaths are not
# swamped by organizations moving between paper and e-file.
pool[, boundary := fcase(
  cohort == "entrant",   fifelse(first_initial, "initial", "no_flag"),
  cohort == "exit",      fifelse(last_final, "final", "no_flag"),
  cohort == "transient", paste0(fifelse(first_initial, "initial", "no_flag"), "-",
                                fifelse(last_final, "final", "no_flag")),
  default = "")]
pool[, stratum := paste(size, form_mix, boundary, sep = " | ")]

# ---- draw -----------------------------------------------------------------------
alloc <- function(pop, n, floor = 25) {
  pmin(pop, pmax(pmin(pop, floor), round(n * sqrt(pop) / sum(sqrt(pop)))))
}
draw <- function(d, n, seed) {
  set.seed(seed)
  cells <- d[, .(pop = .N), by = stratum][, n_draw := alloc(pop, ..n)]
  s <- d[cells, on = "stratum"][, .SD[sample(.N, n_draw[1])], by = stratum]
  s[, weight := pop / n_draw][, c("pop", "n_draw") := NULL]
  s
}

targets <- c(core = 6000, entrant = 1500, exit = 1500, gapped = 1500, transient = 1000)
smp <- rbindlist(lapply(names(targets), function(k)
  draw(pool[cohort == k], targets[[k]], SEED + match(k, names(targets)))), use.names = TRUE)

# split core in half within each stratum
set.seed(SEED)
smp[cohort == "core", half := sample(rep_len(c("dev", "holdout"), .N)), by = stratum]
smp[cohort == "core", `:=`(cohort = paste0("core_", half), weight = weight * 2)]
smp[, half := NULL]

saveRDS(smp, file.path(sd_frame, "sample-orgs.rds"))

cells <- merge(
  pool[, .(population = .N), by = .(cohort = fifelse(cohort == "core", "core", cohort), stratum)],
  smp[, .(drawn = .N), by = .(cohort = sub("_.*", "", cohort), stratum)],
  by = c("cohort", "stratum"), all.x = TRUE)
cells[is.na(drawn), drawn := 0L]
fwrite(cells[order(cohort, stratum)], file.path(sd_repo, "sample-cells.csv"))

print(smp[, .(orgs = .N, org_years = sum(panel_year_count), med_exp = median(med_exp, na.rm = TRUE),
              in_test_panels_1k = sum(in_test_panels_1k)), by = cohort])
print(dcast(smp, size ~ cohort, value.var = "EIN2", fun.aggregate = length))
print(dcast(smp, form_mix ~ cohort, value.var = "EIN2", fun.aggregate = length))
print(smp[cohort %in% c("entrant", "exit", "transient"), .N, by = .(cohort, boundary)][order(cohort, boundary)])
