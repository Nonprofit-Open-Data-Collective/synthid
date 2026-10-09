# 02-frame.R
# Build the population frame: one filing per organization (EIN2) and tax year,
# TY2010-2024, with each organization's panel membership class.
#
# Unit: the latest submission (RETURN_TIME_STAMP) per EIN2 x TAX_YEAR, so an
# amended return replaces the original. Group returns are kept but flagged.
#
# Per filing: form, total expenses (Part I), Part VII people and paid people,
# initial/final-return flags and the principal officer named in the header.
#
# Per organization, panel990's two membership axes over the 2010-2024 window
# (FINDINGS.md D-001):
#   panel_type   persistent | entrant | exit | transient
#   panel_spell  seamless | segmented (one or more interior years missing)
# computed with a vectorized copy of panel990's rules, which is checked against
# panel990::panel_describe() on a random subsample of organizations.
#
# Writes SYNTHID_DIR/frame/filings.rds and SYNTHID_DIR/frame/orgs.rds, and
# dev/devdata/frame-summary.csv (counts by class, committed).
#
#   Rscript dev/devdata/02-frame.R

source("dev/devdata/_config.R")

hdr_cols <- c("EIN2", "OBJECTID", "TAX_YEAR", "RETURN_TYPE", "RETURN_TIME_STAMP",
              "RETURN_AMENDED_X", "RETURN_GROUP_X", "RETURN_PARTIAL_X",
              "F9_00_RETURN_INITIAL_X", "F9_00_RETURN_FINAL_X",
              "F9_00_PRIN_OFF_NAME_PERS", "F9_00_ORG_ADDR_STATE")

fil <- rbindlist(lapply(YEARS, function(y) {
  h  <- as.data.table(read_parquet(eff_path("F9-P00-T00-HEADER", y), col_select = any_of(hdr_cols)))
  p1 <- as.data.table(read_parquet(eff_path("F9-P01-T00-SUMMARY", y),
                                   col_select = c("OBJECTID", "F9_01_EXP_TOT_CY", "F9_01_REV_TOT_CY",
                                                  "F9_01_ACT_GVRN_NUM_VOTE_MEMB", "F9_01_ACT_GVRN_EMPL_TOT")))
  p7 <- as.data.table(read_parquet(eff_path("F9-P07-T01-COMPENSATION", y),
                                   col_select = c("OBJECTID", "F9_07_COMP_DTK_COMP_ORG")))
  s7 <- p7[, .(p7_people = .N, p7_paid = sum(num(F9_07_COMP_DTK_COMP_ORG) > 0, na.rm = TRUE)), by = OBJECTID]
  h <- merge(h, p1[!duplicated(OBJECTID)], by = "OBJECTID", all.x = TRUE)
  h <- merge(h, s7, by = "OBJECTID", all.x = TRUE)
  h[, FILE_YEAR := y]
  stamp("%d: %s filings, %s Part VII rows", y, format(nrow(h), big.mark = ","), format(nrow(p7), big.mark = ","))
  h
}), fill = TRUE)

fil[is.na(p7_people), `:=`(p7_people = 0L, p7_paid = 0L)]
fil[, TAX_YEAR := as.integer(TAX_YEAR)]
stamp("file year != TAX_YEAR on %d rows", fil[TAX_YEAR != FILE_YEAR, .N])

# one filing per organization-year: the latest submission
n_raw <- nrow(fil)
setorder(fil, EIN2, TAX_YEAR, -RETURN_TIME_STAMP)
fil[, n_submissions := .N, by = .(EIN2, TAX_YEAR)]
fil <- fil[!duplicated(fil[, .(EIN2, TAX_YEAR)])]
fil <- fil[TAX_YEAR %in% YEARS]
stamp("%s submissions -> %s org-year filings (%s EINs)", format(n_raw, big.mark = ","),
      format(nrow(fil), big.mark = ","), format(uniqueN(fil$EIN2), big.mark = ","))

fil[, exp := num(F9_01_EXP_TOT_CY)]
fil[, size := as.character(cut(exp, c(-Inf, 0.5, 150000, 1e6, 1e7, 1e8, Inf), right = FALSE,
              labels = c("$0", "$1-150k", "$150k-1m", "$1m-10m", "$10m-100m", "$100m+")))]
fil[is.na(exp), size := "missing"]

# ---- membership classes (vectorized panel990 rules) ---------------------------
first_y <- min(YEARS); last_y <- max(YEARS)
org <- fil[, .(
  panel_year_first = min(TAX_YEAR), panel_year_last = max(TAX_YEAR),
  panel_year_count = uniqueN(TAX_YEAR),
  years_partvii    = sum(p7_people > 0),
  years_990        = sum(RETURN_TYPE == "990"),
  years_990ez      = sum(RETURN_TYPE == "990EZ"),
  any_group        = any(RETURN_GROUP_X %in% c("X", "1", "true", "TRUE")),
  any_initial      = any(F9_00_RETURN_INITIAL_X %in% c("X", "1", "true", "TRUE")),
  any_final        = any(F9_00_RETURN_FINAL_X %in% c("X", "1", "true", "TRUE")),
  med_exp          = median(exp, na.rm = TRUE),
  max_paid         = max(p7_paid),
  med_people       = as.numeric(median(p7_people))
), by = EIN2]

org[, panel_type := fcase(
  panel_year_first == first_y & panel_year_last == last_y, "persistent",
  panel_year_first >  first_y & panel_year_last == last_y, "entrant",
  panel_year_first == first_y & panel_year_last <  last_y, "exit",
  default = "transient")]
org[, span := panel_year_last - panel_year_first + 1L]
org[, panel_spell := fifelse(span == panel_year_count, "seamless", "segmented")]
org[, panel_gap_years := span - panel_year_count]

# gap count and longest gap, from the gaps between consecutive filing years
gp <- fil[order(EIN2, TAX_YEAR), .(d = diff(TAX_YEAR)), by = EIN2][d > 1]
gp <- gp[, .(panel_gap_count = .N, panel_gap_size_max = max(d) - 1L), by = EIN2]
org <- merge(org, gp, by = "EIN2", all.x = TRUE)
org[is.na(panel_gap_count), `:=`(panel_gap_count = 0L, panel_gap_size_max = 0L)]

org[, form_mix := fcase(years_990ez == 0, "990", years_990 == 0, "990EZ", default = "switcher")]
org[, size := as.character(cut(med_exp, c(-Inf, 0.5, 150000, 1e6, 1e7, 1e8, Inf), right = FALSE,
              labels = c("$0", "$1-150k", "$150k-1m", "$1m-10m", "$10m-100m", "$100m+")))]
org[is.na(med_exp), size := "missing"]

# ---- check against panel990 on a subsample -------------------------------------
set.seed(SEED)
chk_ids <- sample(org$EIN2, 20000)
pd <- panel990::panel_describe(as.data.frame(fil[EIN2 %in% chk_ids, .(EIN2, TAX_YEAR)]),
                               time = "TAX_YEAR", id = "EIN2", print = FALSE)
ref <- as.data.table(attr(pd, "classification"))
cmp <- merge(ref, org[, .(EIN2, t2 = panel_type, s2 = panel_spell, g2 = panel_gap_count,
                          m2 = panel_gap_size_max)], by = "EIN2")
# panel990 sets the window from the observed years; it equals YEARS on any
# sample this size, which the check confirms.
stopifnot(identical(as.integer(attr(attr(pd, "classification"), "panel_years")), YEARS))
agree <- cmp[, mean(panel_type == t2 & panel_spell == s2 &
                    panel_gap_count == g2 & panel_gap_size_max == m2)]
stamp("agreement with panel990::panel_describe on %d orgs: %.4f", nrow(cmp), agree)
stopifnot(agree == 1)

saveRDS(fil, file.path(sd_frame, "filings.rds"))
saveRDS(org, file.path(sd_frame, "orgs.rds"))

summ <- org[, .(orgs = .N, with_partvii_all_years = sum(years_partvii == panel_year_count),
                med_years = as.numeric(median(panel_year_count))), by = .(panel_type, panel_spell)][order(panel_type, panel_spell)]
fwrite(summ, file.path(sd_repo, "frame-summary.csv"))
print(summ)
print(org[, .N, by = .(panel_type, form_mix)][order(panel_type, form_mix)])
