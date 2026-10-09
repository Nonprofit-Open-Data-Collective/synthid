# 04-extract.R
# Pull every table for the sampled organizations (03-sample.R) and write the
# development dataset as four related parquet tables in SYNTHID_DIR/devdata:
#
#   orgs.parquet      one row per organization: cohort, stratum, weight,
#                     membership class, overlap flags, BMF fields
#   filings.parquet   one row per organization-year (the filing chosen in
#                     02-frame.R): header, Part I (990 and EZ), Part VI (990
#                     and EZ), Part VII counts (DIR-TRUST-KEY)
#   persons.parquet   Part VII Section A, one row per listed person, with
#                     every source column
#   schedj.parquet    Schedule J Part II, one row per person reported
#
# Only the chosen filing per organization-year is kept (an amended return
# replaces the original), so persons and filings join on OBJECTID.
# Every row carries EIN2, TAX_YEAR and cohort.
#
#   Rscript dev/devdata/04-extract.R

source("dev/devdata/_config.R")
smp <- readRDS(file.path(sd_frame, "sample-orgs.rds"))
fil <- readRDS(file.path(sd_frame, "filings.rds"))
fil <- fil[EIN2 %in% smp$EIN2]
keep_obj <- fil$OBJECTID
stamp("%d orgs, %d filings", nrow(smp), nrow(fil))

# source columns repeated in every efile table; kept once, from the header
common <- c("ORG_EIN", "ORG_NAME_L1", "ORG_NAME_L2", "RETURN_AMENDED_X", "RETURN_GROUP_X",
            "RETURN_PARTIAL_X", "RETURN_TAXPER_DAYS", "RETURN_TIME_STAMP", "RETURN_TYPE",
            "TAX_PERIOD_BEGIN_DATE", "TAX_PERIOD_END_DATE", "TAX_YEAR", "URL", "VERSION")

read_sel <- function(table, y) {
  ds <- open_dataset(eff_path(table, y))
  as.data.table(dplyr::collect(dplyr::filter(ds, OBJECTID %in% keep_obj)))
}
read_all <- function(table) rbindlist(lapply(YEARS, read_sel, table = table), fill = TRUE)

# ---- filings: header + the 1x1 tables, joined on OBJECTID ----------------------
one2one <- c("F9-P01-T00-SUMMARY", "F9-P01-T00-SUMMARY-EZ", "F9-P06-T00-GOVERNANCE",
             "F9-P06-T00-GOVERNANCE-EZ", "F9-P07-T00-DIR-TRUST-KEY")
filings <- read_all("F9-P00-T00-HEADER")
stopifnot(!anyDuplicated(filings$OBJECTID))
for (t in one2one) {
  x <- read_all(t)
  x <- x[, setdiff(names(x), c(common, "EIN2")), with = FALSE]
  dup <- x[duplicated(OBJECTID), .N]
  if (dup) stamp("%s: %d duplicate OBJECTID rows dropped", t, dup)
  x <- x[!duplicated(OBJECTID)]
  clash <- setdiff(intersect(names(x), names(filings)), "OBJECTID")
  if (length(clash)) setnames(x, clash, paste0(clash, "__", sub("^F9-(P\\d+).*?(-EZ)?$", "\\1\\2", t, perl = TRUE)))
  filings <- merge(filings, x, by = "OBJECTID", all.x = TRUE)
  stamp("%s: %d of %d filings matched", t, nrow(x), nrow(filings))
}
filings[, TAX_YEAR := as.integer(TAX_YEAR)]
filings <- merge(filings, fil[, .(OBJECTID, n_submissions, p7_people, p7_paid, size_year = size)],
                 by = "OBJECTID", all.x = TRUE)
filings <- merge(filings, smp[, .(EIN2, cohort)], by = "EIN2")

# ---- persons and Schedule J -------------------------------------------------------
persons <- read_all("F9-P07-T01-COMPENSATION")
persons[, TAX_YEAR := as.integer(TAX_YEAR)]
persons <- merge(persons, smp[, .(EIN2, cohort)], by = "EIN2")
schedj  <- read_all("SJ-P02-T01-COMPENSATION-DTK")
schedj[, TAX_YEAR := as.integer(TAX_YEAR)]
schedj  <- merge(schedj, smp[, .(EIN2, cohort)], by = "EIN2")

# ---- orgs + BMF ---------------------------------------------------------------------
bmf <- as.data.table(panel990::bmf_retrieve(eins = smp$EIN2, format = "parquet",
                                             path = file.path(sd_root, "bmf"), verbose = FALSE))
orgs <- merge(smp, bmf, by = "EIN2", all.x = TRUE)
stamp("BMF matched %d of %d orgs (vintage %s)", orgs[!is.na(bmf_source), .N], nrow(orgs),
      paste(unique(na.omit(orgs$bmf_vintage_ym)), collapse = ","))

# ---- checks + write -------------------------------------------------------------------
stopifnot(all(persons$OBJECTID %in% filings$OBJECTID),
          all(schedj$OBJECTID %in% filings$OBJECTID),
          !anyDuplicated(filings[, .(EIN2, TAX_YEAR)]))
for (nm in c("orgs", "filings", "persons", "schedj"))
  write_parquet(get(nm), file.path(sd_data, paste0(nm, ".parquet")))

sizes <- data.table(table = c("orgs", "filings", "persons", "schedj"),
                    rows = c(nrow(orgs), nrow(filings), nrow(persons), nrow(schedj)),
                    cols = c(ncol(orgs), ncol(filings), ncol(persons), ncol(schedj)))
print(sizes)
print(persons[, .(persons = .N, filings = uniqueN(OBJECTID), orgs = uniqueN(EIN2)), by = cohort])
