# 06-profile.R
# Profile the development dataset: coverage of each table and of the
# independent checks (principal officer, voting members, Schedule J), by
# cohort and form. Writes dev/devdata/profile-*.csv (committed).
#
#   Rscript dev/devdata/06-profile.R

source("dev/devdata/_config.R")
orgs    <- as.data.table(read_parquet(file.path(sd_data, "orgs.parquet")))
filings <- as.data.table(read_parquet(file.path(sd_data, "filings.parquet")))
persons <- as.data.table(read_parquet(file.path(sd_data, "persons.parquet"),
             col_select = c("OBJECTID", "TABLE_ID", "F9_07_COMP_DTK_NAME_PERS", "F9_07_COMP_DTK_NAME_ORG_L1",
                            "F9_07_COMP_DTK_POS_INDIV_TRUST_X", "F9_07_COMP_DTK_COMP_ORG")))
schedj  <- as.data.table(read_parquet(file.path(sd_data, "schedj.parquet"),
             col_select = c("OBJECTID", "SJ_02_COMP_DTK_NAME_PERS")))

has <- function(x) !is.na(x) & nzchar(trimws(as.character(x)))
filings[, form := RETURN_TYPE]

# ---- per filing --------------------------------------------------------------------
p7 <- persons[, .(p7_n = .N,
                  p7_trustee = sum(F9_07_COMP_DTK_POS_INDIV_TRUST_X %in% c("X", "1", "true", "TRUE")),
                  p7_orgname = sum(!has(F9_07_COMP_DTK_NAME_PERS) & has(F9_07_COMP_DTK_NAME_ORG_L1))), by = OBJECTID]
sj <- schedj[, .(sj_n = .N), by = OBJECTID]
filings <- merge(filings, p7, by = "OBJECTID", all.x = TRUE)
filings <- merge(filings, sj, by = "OBJECTID", all.x = TRUE)
filings[is.na(p7_n), p7_n := 0L][is.na(sj_n), sj_n := 0L]
filings[, vote1 := num(F9_01_ACT_GVRN_NUM_VOTE_MEMB)]
filings[, vote6 := num(F9_06_GVRN_NUM_VOTING_MEMB)]

prof <- filings[, .(
  filings            = .N,
  med_people         = as.numeric(median(p7_n)),
  any_paid           = round(mean(p7_paid > 0), 3),
  prin_officer_named = round(mean(has(F9_00_PRIN_OFF_NAME_PERS)), 3),
  voting_members_p1  = round(mean(!is.na(vote1)), 3),
  voting_members_p6  = round(mean(!is.na(vote6)), 3),
  p1_eq_p6           = round(mean(vote1 == vote6, na.rm = TRUE), 3),
  trustee_box_eq_vote = round(mean(p7_trustee == vote6, na.rm = TRUE), 3),
  trustee_box_within2 = round(mean(abs(p7_trustee - vote6) <= 2, na.rm = TRUE), 3),
  schedj_filed       = round(mean(sj_n > 0), 3),
  institution_rows   = round(sum(p7_orgname) / sum(p7_n), 4),
  amended_replaced   = round(mean(n_submissions > 1), 3)
), by = .(cohort, form)][order(cohort, form)]
fwrite(prof, file.path(sd_repo, "profile-filings.csv"))
print(prof)

# ---- per organization ----------------------------------------------------------------
po <- orgs[, .(orgs = .N, bmf_matched = round(mean(!is.na(bmf_source)), 3),
               med_years = as.numeric(median(panel_year_count)),
               med_gap = as.numeric(median(panel_gap_years)),
               first_initial = round(mean(first_initial), 3), last_final = round(mean(last_final), 3),
               in_test_panels_1k = sum(in_test_panels_1k)), by = cohort][order(cohort)]
fwrite(po, file.path(sd_repo, "profile-orgs.csv"))
print(po)

# filings with no Part I summary row
print(filings[is.na(F9_01_EXP_TOT_CY), .N, by = .(form, TAX_YEAR)][order(form, TAX_YEAR)])
