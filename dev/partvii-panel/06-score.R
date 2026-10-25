# 06-score.R
# Score the full-panel linkage (05-link.R) with the checks of
# dev/devdata/08-score.R, so the result compares with the core_dev baseline
# (FINDINGS.md F-007, baseline-core_dev.csv):
#
# A. Linkage summary: persons, compression, years per person.
# B. Principal officer (filing header) vs Part VII: found, is the CEO, and
#    B3 linkage: in consecutive years of one org the principal officer is
#    the same person (same parsed first and last name) -> same EMP_ID
#    (recall), or clearly different people -> different EMP_IDs (false merge).
# C. Board size: step 09 board seats per 990 filing vs voting members.
#    Here the voting members come from Part I line 3a (the frame); the
#    baseline used Part VI line 1a.
# D. CEO continuity: the CEO is the same EMP_ID the next year.
# E. synthid's review flags (flag_links, flag_anomalies), per bucket on a
#    cluster, when the E argument is given.
#
# Every metric is reported for three groups of organizations (column `orgs`):
# all, non_group, and group_return (orgs that ever file a group return,
# 07-orgs.R), and A and D also by form (990 / 990EZ).
#
# Scores the buckets that 05-link.R has finished. Writes
# dev/partvii-panel/score-<tag>.csv (one row per metric, committed).
#
#   Rscript dev/partvii-panel/06-score.R [tag] [E: flags workers, 0 = skip]

source("dev/partvii-panel/_panel.R")
suppressPackageStartupMessages({ library(synthid); library(peopleparser) })
args <- commandArgs(trailingOnly = TRUE)
TAG  <- if (length(args) >= 1) args[1] else "full"
NWE  <- if (length(args) >= 2) as.integer(args[2]) else 0L

pv_map <- file.path(pv_root, "emp-map")
done <- sub("^bucket=", "", list.files(pv_map, pattern = "^bucket="))
stamp("%d linked buckets", length(done))

map <- as.data.table(dplyr::collect(open_dataset(pv_map, partitioning = hive_partition(bucket = utf8()))))
linked_eins <- unique(map$ein)
ppl <- as.data.table(dplyr::collect(dplyr::select(dplyr::filter(open_dataset(pv_ppl), ein %in% linked_eins),
         PERSON_YEAR_ID, OBJECTID, formtype, first_name, last_name, role.final, role.board, trustee.x, org.leadership)))
lk <- merge(map, ppl, by = "PERSON_YEAR_ID")
stopifnot(nrow(lk) == nrow(map))
rm(ppl); gc(verbose = FALSE)
orgs <- as.data.table(read_parquet(file.path(pv_root, "orgs", "orgs.parquet"), col_select = c("ein", "group_return")))
lk[orgs, on = "ein", grp := i.group_return]
stopifnot(!anyNA(lk$grp))
fil <- as.data.table(readRDS(file.path(sd_frame, "filings.rds")))[OBJECTID %in% lk$OBJECTID,
         .(OBJECTID, RETURN_TYPE, F9_00_PRIN_OFF_NAME_PERS, vote = num(F9_01_ACT_GVRN_NUM_VOTE_MEMB))]
stamp("%s records, %s orgs (%s group-return orgs), %s filings", format(nrow(lk), big.mark = ","),
      format(uniqueN(lk$ein), big.mark = ","), format(uniqueN(lk[grp == TRUE, ein]), big.mark = ","),
      format(nrow(fil), big.mark = ","))

M <- list()
put <- function(section, metric, value, n = NA, form = "all", orgs = "all")
  M[[length(M) + 1L]] <<- data.table(section, orgs, form, metric, value = round(value, 4), n)
GROUPS <- list(all = c(TRUE, FALSE), non_group = FALSE, group_return = TRUE)

# ---- principal officer matches (needed by B for every group) ------------------------
f9 <- fil[RETURN_TYPE == "990" & !is.na(F9_00_PRIN_OFF_NAME_PERS) & nzchar(trimws(F9_00_PRIN_OFF_NAME_PERS))]
po_u <- unique(f9$F9_00_PRIN_OFF_NAME_PERS)
stamp("parsing %s principal officer names", format(length(po_u), big.mark = ","))
po_p <- rbindlist(lapply(split(po_u, ceiling(seq_along(po_u) / 1e6)), function(u)
  as.data.table(parse_names(u))[, .(po_raw = u, po_first = first_name, po_last = last_name)]))
f9 <- merge(f9, po_p, by.x = "F9_00_PRIN_OFF_NAME_PERS", by.y = "po_raw")
norm <- function(x) gsub("[^A-Z]", "", toupper(ifelse(is.na(x), "", x)))
cand <- merge(f9[, .(OBJECTID, po_first = norm(po_first), po_last = norm(po_last))],
              lk[OBJECTID %in% f9$OBJECTID, .(OBJECTID, EMP_ID, first = norm(first_name), last = norm(last_name),
                                               role.final, org.leadership, ein, taxyr, grp)],
              by = "OBJECTID", allow.cartesian = TRUE)
cand <- cand[nzchar(po_last) & last == po_last &
             (first == po_first | (nchar(first) == 1 | nchar(po_first) == 1) & substr(first, 1, 1) == substr(po_first, 1, 1))]
cand[, pri := (first == po_first) * 2 + (role.final == "CEO")]
setorder(cand, OBJECTID, -pri)
po_all <- cand[!duplicated(OBJECTID)]
f9 <- merge(f9, unique(lk[, .(OBJECTID, grp)]), by = "OBJECTID")

for (g in names(GROUPS)) {
  G <- GROUPS[[g]]
  x <- lk[grp %in% G]
  if (!nrow(x)) next

  # ---- A. linkage summary -----------------------------------------------------------
  for (fm in c("all", "990", "990EZ")) {
    xf <- if (fm == "all") x else x[formtype == fm]
    if (!nrow(xf)) next
    pers <- xf[, .(years = uniqueN(taxyr)), by = EMP_ID]
    put("A", "records", nrow(xf), form = fm, orgs = g); put("A", "persons", nrow(pers), form = fm, orgs = g)
    put("A", "compression", 1 - nrow(pers) / nrow(xf), form = fm, orgs = g)
    put("A", "persons in 2+ years", mean(pers$years > 1), nrow(pers), form = fm, orgs = g)
    put("A", "median years per multi-year person", median(pers[years > 1, years]), form = fm, orgs = g)
  }
  put("A", "persons with >1 record in one year", x[, .N, by = .(EMP_ID, taxyr)][, mean(N > 1)], orgs = g)
  put("A", "orgs", uniqueN(x$ein), orgs = g)

  # ---- B. principal officer (full 990s) -----------------------------------------------
  f9g <- f9[grp %in% G]; po <- po_all[grp %in% G]
  n990 <- fil[OBJECTID %in% x$OBJECTID & RETURN_TYPE == "990", .N]
  put("B", "990 filings naming a principal officer", nrow(f9g) / n990, n990, orgs = g)
  put("B1", "principal officer found in Part VII", nrow(po) / nrow(f9g), nrow(f9g), orgs = g)
  put("B2", "found principal officer is step 09 CEO", po[, mean(role.final == "CEO")], nrow(po), orgs = g)
  for (ld in c("designated", "imputed", "board_governed", "no_paid"))
    put("B2", sprintf("principal officer is CEO | org.leadership = %s", ld),
        po[org.leadership == ld, mean(role.final == "CEO")], po[org.leadership == ld, .N], orgs = g)
  pr <- merge(po[, .(ein, taxyr, EMP_ID, po_first, po_last)],
              po[, .(ein, taxyr = taxyr - 1L, EMP_ID1 = EMP_ID, po_first1 = po_first, po_last1 = po_last)],
              by = c("ein", "taxyr"))
  same <- pr[po_last == po_last1 & po_first == po_first1]
  diff <- pr[po_last != po_last1 & substr(po_first, 1, 1) != substr(po_first1, 1, 1)]
  put("B3", "same principal officer next year -> same EMP_ID (recall)", same[, mean(EMP_ID == EMP_ID1)], nrow(same), orgs = g)
  put("B3", "different principal officer next year -> same EMP_ID (false merge)", diff[, mean(EMP_ID == EMP_ID1)], nrow(diff), orgs = g)

  # ---- C. board size (full 990s, Part I voting members) -------------------------------
  bd <- x[formtype == "990", .(seats = sum(!is.na(role.board) & nzchar(role.board)),
                                trustee_box = sum(trustee.x %in% c("1", "X", "TRUE", "true"))), by = OBJECTID]
  bd <- merge(bd, fil[, .(OBJECTID, vote)], by = "OBJECTID")[!is.na(vote)]
  for (v in c("seats", "trustee_box")) {
    put("C", sprintf("%s = voting members", v), bd[, mean(get(v) == vote)], nrow(bd), orgs = g)
    put("C", sprintf("%s within 2 of voting members", v), bd[, mean(abs(get(v) - vote) <= 2)], nrow(bd), orgs = g)
    put("C", sprintf("%s median (count - voting members)", v), bd[, median(get(v) - vote)], nrow(bd), orgs = g)
  }

  # ---- D. CEO continuity ----------------------------------------------------------------
  ceo <- unique(x[role.final == "CEO", .(ein, taxyr, EMP_ID, formtype)])
  cp <- merge(unique(ceo[, .(ein, taxyr, formtype)]), unique(ceo[, .(ein, taxyr = taxyr - 1L)]), by = c("ein", "taxyr"))
  kept <- unique(merge(ceo, unique(ceo[, .(ein, taxyr = taxyr - 1L, EMP_ID)]), by = c("ein", "taxyr", "EMP_ID"))[, .(ein, taxyr)])
  cp <- merge(cp, kept[, kept := TRUE], by = c("ein", "taxyr"), all.x = TRUE)[, kept := !is.na(kept)]
  put("D", "CEO is the same EMP_ID next year", cp[, mean(kept)], nrow(cp), orgs = g)
  for (fm in c("990", "990EZ"))
    if (cp[formtype == fm, .N]) put("D", "CEO is the same EMP_ID next year", cp[formtype == fm, mean(kept)], cp[formtype == fm, .N], form = fm, orgs = g)
}

# ---- E. review flags, per bucket ---------------------------------------------------------
if (NWE > 0) {
  stamp("review flags on %d buckets, %d workers", length(done), NWE)
  cl <- parallel::makePSOCKcluster(NWE); on.exit(parallel::stopCluster(cl), add = TRUE)
  fl <- rbindlist(parallel::parLapplyLB(cl, done, function(b, root) {
    suppressPackageStartupMessages({ library(data.table); library(arrow); library(synthid) })
    x <- merge(as.data.table(read_parquet(file.path(root, "people-by-ein", paste0("bucket=", b), "part-0.parquet"))),
               as.data.table(read_parquet(file.path(root, "emp-map", paste0("bucket=", b), "part-0.parquet"),
                                          col_select = c("PERSON_YEAR_ID", "EMP_ID", "EMP_ANCHOR", "EMP_N_RECORDS", "EMP_N_YEARS"))),
               by = "PERSON_YEAR_ID")
    a <- tryCatch(as.data.table(flag_links(as.data.frame(x)))[!is.na(category), .(source = "flag_links", category, emp_id)],
                  error = function(e) data.table(source = "flag_links", category = paste("ERROR:", conditionMessage(e)), emp_id = NA_character_))
    q <- tryCatch(unique(as.data.table(anomaly_review_queue(flag_anomalies(as.data.frame(x))))[, .(source = "flag_anomalies", category, emp_id)]),
                  error = function(e) data.table(source = "flag_anomalies", category = paste("ERROR:", conditionMessage(e)), emp_id = NA_character_))
    rbind(a, q)
  }, root = pv_root))
  emp_grp <- unique(lk[, .(emp_id = EMP_ID, grp)])
  fl <- merge(fl, emp_grp, by = "emp_id", all.x = TRUE)
  for (g in names(GROUPS)) {
    n_pers <- uniqueN(lk[grp %in% GROUPS[[g]], EMP_ID])
    f <- fl[grp %in% GROUPS[[g]] | (g == "all" & is.na(grp)), .(n = uniqueN(emp_id)), by = .(source, category)][order(source, -n)]
    for (r in seq_len(nrow(f))) put("E", sprintf("%s persons: %s", f$source[r], f$category[r]), f$n[r], n_pers, orgs = g)
  }
}

out <- rbindlist(M)
fwrite(out, file.path(pv_repo, sprintf("score-%s.csv", TAG)))
print(dcast(out[form == "all"], section + metric ~ orgs, value.var = "value")[order(section)], nrows = 200)
