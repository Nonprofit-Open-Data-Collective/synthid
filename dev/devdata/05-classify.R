# 05-classify.R
# Run titleclassifier (steps 01-09, with the 990-EZ role model) on the
# development dataset's Part VII rows, then parse names with peopleparser, to
# give synthid its input.
#
# Writes to SYNTHID_DIR/devdata:
#   classified/<year>.parquet  every person-title row (a person with a split
#                              title has one row per title), all source columns
#                              plus the classification fields
#   people.parquet             one row per listed person per filing (the
#                              role.primary row), in synthid_cols() layout plus
#                              the step 09 role columns: the linker's input
#
# A year already classified is skipped, so the script can be re-run.
#
#   Rscript dev/devdata/05-classify.R [workers]

source("dev/devdata/_config.R")
suppressPackageStartupMessages({ library(titleclassifier); library(peopleparser) })
args <- commandArgs(trailingOnly = TRUE)
NW   <- if (length(args)) as.integer(args[1]) else max(1L, parallel::detectCores(logical = FALSE) - 2L)

cls_dir <- file.path(sd_data, "classified")
dir.create(cls_dir, showWarnings = FALSE)
persons <- as.data.table(read_parquet(file.path(sd_data, "persons.parquet")))

for (y in YEARS) {
  f <- file.path(cls_dir, sprintf("%d.parquet", y))
  if (file.exists(f)) { stamp("%d: done already", y); next }
  raw <- as.data.frame(persons[TAX_YEAR == y])
  raw[] <- lapply(raw, function(v) { v <- as.character(v); v[is.na(v)] <- ""; v })
  t0 <- Sys.time()
  # classify_titles() parallelizes over batches of filings; its default of
  # 2,000 filings a batch leaves most workers idle on a year this size.
  bs  <- max(50L, ceiling(uniqueN(raw$OBJECTID) / (4L * NW)))
  out <- suppressMessages(classify_titles(raw, batch_size = bs, workers = NW, preserve_input = TRUE))
  out <- as.data.table(out)
  write_parquet(out, f)
  stamp("%d: %s Part VII rows -> %s person-title rows in %.1f min", y, format(nrow(raw), big.mark = ","),
        format(nrow(out), big.mark = ","), as.numeric(Sys.time() - t0, units = "mins"))
}
rm(persons); gc()

# ---- people: one row per person per filing ----------------------------------------
cls <- rbindlist(lapply(YEARS, function(y) as.data.table(read_parquet(
  file.path(cls_dir, sprintf("%d.parquet", y)),
  col_select = c("OBJECTID", "TABLE_ID", "EIN2", "TAX_YEAR", "cohort", "RETURN_TYPE",
                 "F9_07_COMP_DTK_NAME_PERS", "org.name", "dtk.name", "title.raw", "title.standard",
                 "title.match", "emp.level", "board.role", "tot.hours", "tot.comp",
                 "dtk.indiv.trustee.x", "dtk.officer.x", "dtk.key.empl.x", "dtk.high.comp.x", "dtk.former.x",
                 "role.position", "role.final", "role.board", "role.ceo", "role.source", "role.method",
                 "role.primary", "org.leadership")))))
stamp("%s person-title rows", format(nrow(cls), big.mark = ","))

pp <- cls[role.primary %in% c(TRUE, "TRUE")]
dup <- pp[duplicated(pp[, .(OBJECTID, TABLE_ID)]), .N]
miss <- uniqueN(cls[, .(OBJECTID, TABLE_ID)]) - uniqueN(pp[, .(OBJECTID, TABLE_ID)])
stamp("role.primary rows: %s; duplicate person keys: %d; persons with no primary row: %d",
      format(nrow(pp), big.mark = ","), dup, miss)
if (dup) pp <- pp[!duplicated(pp[, .(OBJECTID, TABLE_ID)])]
if (miss) {   # fall back to the first title row for any person without a primary row
  extra <- cls[!pp, on = .(OBJECTID, TABLE_ID)][!duplicated(cls[!pp, on = .(OBJECTID, TABLE_ID)][, .(OBJECTID, TABLE_ID)])]
  pp <- rbind(pp, extra)
}

# parse_names() is a pure per-string map (sanitize_input() and parse_name()
# see one name at a time), so parsing the distinct strings and joining back is
# exact. Names repeat 3.7x in this dataset and 6.4x across all of Part VII.
t0 <- Sys.time()
uniq   <- unique(pp$dtk.name)
parsed <- as.data.table(peopleparser::parse_names(uniq))
parsed <- parsed[match(pp$dtk.name, uniq)]
stamp("parsed %s distinct names for %s rows in %.1f min", format(length(uniq), big.mark = ","),
      format(nrow(pp), big.mark = ","), as.numeric(Sys.time() - t0, units = "mins"))

people <- data.table(
  PERSON_YEAR_ID = synthid::person_year_id(pp$OBJECTID, pp$TABLE_ID),
  OBJECTID = as.character(pp$OBJECTID), TABLE_ID = as.character(pp$TABLE_ID),
  ein = pp$EIN2, taxyr = as.integer(pp$TAX_YEAR), cohort = pp$cohort, formtype = pp$RETURN_TYPE,
  org.name = pp$org.name,
  name = parsed$name, name.raw = pp$F9_07_COMP_DTK_NAME_PERS,
  salutation = parsed$salutation, first_name = parsed$first_name, middle_name = parsed$middle_name,
  last_name = parsed$last_name, suffix = parsed$suffix, gender = parsed$gender,
  title.standard = pp$title.standard, title.raw = pp$title.raw, title.match = pp$title.match,
  emp.level = pp$emp.level, board.role = pp$board.role,
  tot.hours = num(pp$tot.hours), tot.comp = num(pp$tot.comp),
  trustee.x = pp$dtk.indiv.trustee.x, officer.x = pp$dtk.officer.x, key.empl.x = pp$dtk.key.empl.x,
  high.comp.x = pp$dtk.high.comp.x, former.x = pp$dtk.former.x,
  role.position = pp$role.position, role.final = pp$role.final, role.board = pp$role.board,
  role.ceo = pp$role.ceo, role.source = pp$role.source, role.method = pp$role.method,
  org.leadership = pp$org.leadership
)
setorder(people, ein, taxyr, OBJECTID, TABLE_ID)
write_parquet(people, file.path(sd_data, "people.parquet"))

print(people[, .(people = .N, orgs = uniqueN(ein)), by = cohort])
print(people[, .N, by = .(formtype, role.final)][, share := round(N / sum(N), 3), by = formtype][order(formtype, -N)])
print(people[, .N, by = role.method])
