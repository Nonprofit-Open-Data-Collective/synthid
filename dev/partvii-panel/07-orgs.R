# 07-orgs.R
# One row per organization in the panel, with the flags an analysis needs to
# include or set apart a kind of organization. For now that is group returns.
#
# A group return (RETURN_GROUP_X in the filing header) is one 990 that a
# central organization files for its subordinate chapters, so its Part VII
# lists officers from many chapters. Group returns are kept in the linkage
# (2026-10-23) and flagged here: 1,292 orgs (0.16%) ever file one, holding
# 0.8% of the records but 22% of the linkage cost, and 8 of the 12 largest
# orgs (Hadassah's group return lists about 2,600 people a year). Officers of
# different chapters can share a name, so expect more false merges there.
#
# Writes SYNTHID_DIR/partvii-v3_1/orgs/orgs.parquet:
#   ein, org.name (latest year), first_year, last_year, n_years, n_filings,
#   n_records (Part VII people), max_records_year,
#   group_return (ever filed one), group_return_years (how many years),
#   group_return_all (every filing is one), bucket
#
#   Rscript dev/partvii-panel/07-orgs.R

source("dev/partvii-panel/_panel.R")
pv_org <- file.path(pv_root, "orgs"); dir.create(pv_org, showWarnings = FALSE)
yes <- function(x) toupper(trimws(x)) %in% c("TRUE", "1", "X")

fr <- as.data.table(readRDS(file.path(sd_frame, "filings.rds")))[,
        .(ein = EIN2, taxyr = TAX_YEAR, grp = yes(RETURN_GROUP_X))]
pp <- as.data.table(dplyr::collect(dplyr::select(open_dataset(pv_ppl), ein, taxyr, org.name)))
recs <- pp[, .(n_records = .N, max_records_year = max(table(taxyr)),
               org.name = org.name[which.max(taxyr)]), by = ein]
orgs <- fr[ein %in% recs$ein, .(first_year = min(taxyr), last_year = max(taxyr), n_years = uniqueN(taxyr),
                                n_filings = .N, group_return = any(grp), group_return_years = sum(grp),
                                group_return_all = all(grp)), by = ein]
orgs <- merge(orgs, recs, by = "ein")
orgs[, bucket := substr(ein, 13L, 14L)]
setcolorder(orgs, c("ein", "org.name", "first_year", "last_year", "n_years", "n_filings", "n_records",
                    "max_records_year", "group_return", "group_return_years", "group_return_all", "bucket"))
setorder(orgs, ein)
stopifnot(!anyDuplicated(orgs$ein), nrow(orgs) == uniqueN(pp$ein))
write_parquet(orgs, file.path(pv_org, "orgs.parquet"))

s <- orgs[, .(orgs = .N, records = sum(n_records), cost = sum(as.numeric(n_records)^2)), by = group_return]
s[, `:=`(orgs_share = orgs / sum(orgs), records_share = records / sum(records), cost_share = cost / sum(cost))]
fwrite(s[, .(group_return, orgs, records, orgs_share = round(orgs_share, 4), records_share = round(records_share, 4),
             cost_share = round(cost_share, 4))], file.path(pv_repo, "group-return-summary.csv"))
print(s)
stamp("orgs.parquet: %s orgs, %s ever file a group return (%s every year)", format(nrow(orgs), big.mark = ","),
      format(orgs[group_return == TRUE, .N], big.mark = ","), format(orgs[group_return_all == TRUE, .N], big.mark = ","))
