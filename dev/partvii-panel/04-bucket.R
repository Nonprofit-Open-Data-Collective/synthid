# 04-bucket.R
# Re-lay the people tables (02-people.R) for linkage: one file per EIN bucket,
# every tax year of an organization in the same file, so each file is a
# self-contained panel that a worker can link on its own.
#
# Bucket: the last two digits of the EIN (00-99). They are close to uniform
# (each bucket holds 0.96-1.1% of organizations); the leading two digits are
# IRS prefix codes and are not (0.0004-4.3%).
#
# Each file is sorted by ein, taxyr, OBJECTID, TABLE_ID and written in row
# groups of ROW_GROUP rows, so a query for particular organizations reads only
# the row groups that hold them. Columns: the linker's inputs (synthid_cols())
# plus the fields the anomaly scan, transitions and role passes read, and
# org.state, org.cbsa and org.ntee, which the cross-organization blocking passes
# need (05-link.R). Every other column joins back from people/ or classified/ on
# PERSON_YEAR_ID (or OBJECTID + TABLE_ID).
#
# Writes SYNTHID_DIR/partvii-v3_1/people-by-ein/bucket=NN/part-0.parquet
#
#   Rscript dev/partvii-panel/04-bucket.R

source("dev/partvii-panel/_panel.R")
build_step("04-bucket")   # build-log.csv
ROW_GROUP <- 100000L
pv_bkt <- file.path(pv_root, "people-by-ein")

LINKER_COLS <- c(
  "PERSON_YEAR_ID", "OBJECTID", "TABLE_ID", "ein", "taxyr", "formtype",
  "org.name", "org.state", "org.cbsa", "org.ntee",
  "name", "name.raw", "salutation", "first_name", "middle_name", "last_name", "suffix", "gender",
  "title.standard", "title.raw", "tot.hours", "tot.comp",
  "trustee.x", "officer.x", "role.final", "role.board", "role.ceo")

missing <- YEARS[!file.exists(ppl_file(YEARS))]
if (length(missing)) stop("people years missing: ", paste(missing, collapse = " "))

t0 <- Sys.time()
ppl <- rbindlist(lapply(YEARS, function(y) as.data.table(read_parquet(ppl_file(y), col_select = all_of(LINKER_COLS)))))
stamp("read %s people rows", format(nrow(ppl), big.mark = ","))

stopifnot(all(grepl("^EIN-\\d{2}-\\d{7}$", ppl$ein)))
ppl[, bucket := substr(ein, 13L, 14L)]
setorder(ppl, bucket, ein, taxyr, OBJECTID, TABLE_ID)

unlink(pv_bkt, recursive = TRUE)
summ <- ppl[, {
  d <- file.path(pv_bkt, paste0("bucket=", .BY$bucket))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  write_parquet(.SD, file.path(d, "part-0.parquet"), chunk_size = ROW_GROUP)
  .(rows = .N, orgs = uniqueN(ein), years = uniqueN(taxyr))
}, by = bucket, .SDcols = LINKER_COLS]

fwrite(summ, file.path(pv_repo, "bucket-summary.csv"))
stamp("wrote %d buckets in %.1f min: %s-%s rows each, %s-%s orgs each", nrow(summ),
      as.numeric(Sys.time() - t0, units = "mins"),
      format(min(summ$rows), big.mark = ","), format(max(summ$rows), big.mark = ","),
      format(min(summ$orgs), big.mark = ","), format(max(summ$orgs), big.mark = ","))
