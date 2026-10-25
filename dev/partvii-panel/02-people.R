# 02-people.R
# Build the linker's input from the classified years (01-classify.R): one row
# per listed person per filing (the role.primary title row), with names parsed
# by peopleparser, in the same layout as dev/devdata's people.parquet (without
# the dev dataset's cohort column).
#
# parse_names() is a pure per-string map, so the distinct name strings across
# all years are parsed once and joined back (exact).
#
# Writes SYNTHID_DIR/partvii-v3_1/people/people-<year>.parquet and
# dev/partvii-panel/panel-summary.csv (committed).
#
#   Rscript dev/partvii-panel/02-people.R

source("dev/partvii-panel/_panel.R")
suppressPackageStartupMessages(library(peopleparser))

missing <- YEARS[!file.exists(cls_file(YEARS))]
if (length(missing)) stop("classified years missing: ", paste(missing, collapse = " "))
fwrite(pkg_info(), file.path(pv_root, "build-info-people.csv"))

keep <- c("OBJECTID", "TABLE_ID", "EIN2", "TAX_YEAR", "RETURN_TYPE",
          "F9_07_COMP_DTK_NAME_PERS", "org.name", "dtk.name", "title.raw", "title.standard",
          "title.match", "emp.level", "board.role", "tot.hours", "tot.comp",
          "dtk.indiv.trustee.x", "dtk.officer.x", "dtk.key.empl.x", "dtk.high.comp.x", "dtk.former.x",
          "role.position", "role.final", "role.board", "role.ceo", "role.source", "role.method",
          "role.primary", "org.leadership", "org.type", "org.type.source")

# ---- one row per person per filing, per year ------------------------------------
prim <- function(y) {
  cls <- as.data.table(read_parquet(cls_file(y), col_select = all_of(keep)))
  pp  <- cls[role.primary %in% c(TRUE, "TRUE")]
  pp  <- pp[!duplicated(pp[, .(OBJECTID, TABLE_ID)])]
  # any person without a primary row falls back to their first title row
  rest <- cls[!pp, on = .(OBJECTID, TABLE_ID)]
  rest <- rest[!duplicated(rest[, .(OBJECTID, TABLE_ID)])]
  stamp("%d: %s title rows -> %s persons (%s without a role.primary row)", y,
        format(nrow(cls), big.mark = ","), format(nrow(pp) + nrow(rest), big.mark = ","),
        format(nrow(rest), big.mark = ","))
  rbind(pp, rest)
}
pp_by_year <- lapply(setNames(YEARS, YEARS), prim)

# ---- parse the distinct names once, in parallel -----------------------------------
t0   <- Sys.time()
uniq <- unique(unlist(lapply(pp_by_year, `[[`, "dtk.name"), use.names = FALSE))
stamp("%s distinct names across %s person rows", format(length(uniq), big.mark = ","),
      format(sum(vapply(pp_by_year, nrow, 1L)), big.mark = ","))
NAME_COLS <- c("name", "salutation", "first_name", "middle_name", "last_name", "suffix", "gender")
parse_chunked <- function(u) {
  # parse_names() runs its own furrr multisession plan over all cores, and its
  # workers find census.names only when peopleparser is attached.
  ch <- split(u, ceiling(seq_along(u) / 1e6))
  rbindlist(lapply(seq_along(ch), function(i) {
    p <- as.data.table(parse_names(ch[[i]]))[, ..NAME_COLS]
    stamp("  names chunk %d of %d parsed", i, length(ch))
    p
  }))
}
# Reuse an earlier build's parsed names (REUSE_NAMES_FROM: its people/ folder).
# The parsed name depends only on dtk.name (titleclassifier step 01) and
# peopleparser, so when neither changed the earlier result is exact; a
# random 50,000 reused names are parsed again and must match.
reuse_dir <- Sys.getenv("REUSE_NAMES_FROM")
parsed <- data.table(dtk.name = uniq)
if (nzchar(reuse_dir)) {
  old <- as.data.table(dplyr::collect(dplyr::select(open_dataset(reuse_dir), PERSON_YEAR_ID, all_of(NAME_COLS))))
  key <- rbindlist(lapply(pp_by_year, function(p)
    data.table(PERSON_YEAR_ID = synthid::person_year_id(p$OBJECTID, p$TABLE_ID), dtk.name = p$dtk.name)))
  old <- merge(key, old, by = "PERSON_YEAR_ID")[!duplicated(dtk.name)][, PERSON_YEAR_ID := NULL]
  rm(key); gc(verbose = FALSE)
  set.seed(SEED); chk <- old[sample(.N, min(.N, 50000L))]
  fresh <- as.data.table(parse_names(chk$dtk.name))[, ..NAME_COLS]
  ok <- isTRUE(all.equal(as.data.frame(chk[, ..NAME_COLS]), as.data.frame(fresh), check.attributes = FALSE))
  stamp("reusing parsed names from %s: %s of %s distinct names; 50,000 re-parsed match: %s", reuse_dir,
        format(nrow(old), big.mark = ","), format(length(uniq), big.mark = ","), ok)
  if (!ok) stop("re-parsed names differ from the reused ones; unset REUSE_NAMES_FROM")
  parsed <- merge(parsed, old, by = "dtk.name", all.x = TRUE, sort = FALSE)
}
todo <- if ("name" %in% names(parsed)) parsed[is.na(name) & !(dtk.name %in% old$dtk.name), dtk.name] else uniq
if (length(todo)) {
  stamp("parsing %s names", format(length(todo), big.mark = ","))
  fresh <- cbind(data.table(dtk.name = todo), parse_chunked(todo))
  parsed <- rbind(parsed[!dtk.name %in% todo], fresh, fill = TRUE)
}
parsed <- parsed[match(uniq, dtk.name)]
stopifnot(nrow(parsed) == length(uniq), identical(parsed$dtk.name, uniq))
stamp("parsed names in %.1f min", as.numeric(Sys.time() - t0, units = "mins"))

# ---- write people-<year> ------------------------------------------------------------
summ <- rbindlist(lapply(names(pp_by_year), function(yy) {
  pp <- pp_by_year[[yy]]
  pn <- parsed[match(pp$dtk.name, uniq)]
  people <- data.table(
    PERSON_YEAR_ID = synthid::person_year_id(pp$OBJECTID, pp$TABLE_ID),
    OBJECTID = as.character(pp$OBJECTID), TABLE_ID = as.character(pp$TABLE_ID),
    ein = pp$EIN2, taxyr = as.integer(pp$TAX_YEAR), formtype = pp$RETURN_TYPE,
    org.name = pp$org.name,
    name = pn$name, name.raw = pp$F9_07_COMP_DTK_NAME_PERS,
    salutation = pn$salutation, first_name = pn$first_name, middle_name = pn$middle_name,
    last_name = pn$last_name, suffix = pn$suffix, gender = pn$gender,
    title.standard = pp$title.standard, title.raw = pp$title.raw, title.match = pp$title.match,
    emp.level = pp$emp.level, board.role = pp$board.role,
    tot.hours = num(pp$tot.hours), tot.comp = num(pp$tot.comp),
    trustee.x = pp$dtk.indiv.trustee.x, officer.x = pp$dtk.officer.x, key.empl.x = pp$dtk.key.empl.x,
    high.comp.x = pp$dtk.high.comp.x, former.x = pp$dtk.former.x,
    role.position = pp$role.position, role.final = pp$role.final, role.board = pp$role.board,
    role.ceo = pp$role.ceo, role.source = pp$role.source, role.method = pp$role.method,
    org.leadership = pp$org.leadership, org.type = pp$org.type, org.type.source = pp$org.type.source
  )
  setorder(people, ein, OBJECTID, TABLE_ID)
  stopifnot(!anyDuplicated(people$PERSON_YEAR_ID))
  write_parquet(people, ppl_file(as.integer(yy)))
  people[, .(persons = .N, filings = uniqueN(OBJECTID), orgs = uniqueN(ein),
             no_last_name = sum(is.na(last_name) | last_name == ""),
             no_title_standard = sum(is.na(title.standard) | title.standard == ""),
             board = sum(role.final == "BOARD", na.rm = TRUE), ceo = sum(role.final == "CEO", na.rm = TRUE),
             officer = sum(role.final == "OFFICER", na.rm = TRUE)), by = .(taxyr)]
}))
fwrite(summ, file.path(pv_repo, "panel-summary.csv"))
print(summ)
stamp("people written for %d years", nrow(summ))
