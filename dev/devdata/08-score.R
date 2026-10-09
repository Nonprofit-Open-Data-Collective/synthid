# 08-score.R
# Score a linked cohort (07-link.R) against the checks the filing tables give
# (FINDINGS.md F-004). Full-990 filings only, where the checks exist.
#
# A. Linkage summary: persons, compression, years per person.
# B. Principal officer (header) vs Part VII:
#    B1 is the principal officer listed in Part VII of the same filing?
#    B2 is that person step 09's CEO?
#    B3 linkage: in consecutive years of one organization, the principal
#       officer is
#       - the same person (same parsed first and last name): their two
#         Part VII records should share an EMP_ID (a recall check);
#       - clearly different people (different last and first name): their
#         records should not (a false-merge check).
# C. Board size: step 09's board seats per filing vs the Part VI voting
#    members, with the Part VII trustee-box count as the comparison baseline.
# D. CEO continuity: is the CEO the same EMP_ID the next year?
# E. synthid's review flags: flag_links() and flag_anomalies() counts.
#
# A principal officer is matched to a Part VII person by parsed last name and
# first name (or first initial when either side gives only an initial).
#
# Writes dev/devdata/baseline-<tag>.csv (one row per metric, committed).
#
#   Rscript dev/devdata/08-score.R [tag]

source("dev/devdata/_config.R")
suppressPackageStartupMessages({ library(synthid); library(peopleparser) })
args <- commandArgs(trailingOnly = TRUE)
TAG  <- if (length(args)) args[1] else "core_dev"

lk  <- as.data.table(read_parquet(file.path(sd_root, "linked", paste0(TAG, ".parquet"))))
fil <- as.data.table(read_parquet(file.path(sd_data, "filings.parquet"),
         col_select = c("OBJECTID", "EIN2", "TAX_YEAR", "RETURN_TYPE", "F9_00_PRIN_OFF_NAME_PERS",
                        "F9_06_GVRN_NUM_VOTING_MEMB")))
fil <- fil[OBJECTID %in% lk$OBJECTID]
M <- list()
put <- function(section, metric, value, n = NA) M[[length(M) + 1L]] <<- data.table(section, metric, value = round(value, 4), n)

# ---- A. linkage summary -------------------------------------------------------------
pers <- lk[, .(years = uniqueN(taxyr), recs = .N), by = EMP_ID]
put("A", "records", nrow(lk)); put("A", "persons", nrow(pers))
put("A", "compression", 1 - nrow(pers) / nrow(lk))
put("A", "persons in 2+ years", mean(pers$years > 1), nrow(pers))
put("A", "persons with >1 record in one year", lk[, .N, by = .(EMP_ID, taxyr)][, mean(N > 1)])
put("A", "median years per multi-year person", median(pers[years > 1, years]))

# ---- B. principal officer -------------------------------------------------------------
f9 <- fil[RETURN_TYPE == "990" & !is.na(F9_00_PRIN_OFF_NAME_PERS) & nzchar(trimws(F9_00_PRIN_OFF_NAME_PERS))]
put("B", "990 filings naming a principal officer", nrow(f9) / fil[RETURN_TYPE == "990", .N], fil[RETURN_TYPE == "990", .N])
po_u <- unique(f9$F9_00_PRIN_OFF_NAME_PERS)
po_p <- as.data.table(peopleparser::parse_names(po_u))[, .(po_raw = po_u, po_first = first_name, po_last = last_name)]
f9 <- merge(f9, po_p, by.x = "F9_00_PRIN_OFF_NAME_PERS", by.y = "po_raw")

norm <- function(x) gsub("[^A-Z]", "", toupper(ifelse(is.na(x), "", x)))
cand <- merge(f9[, .(OBJECTID, po_first = norm(po_first), po_last = norm(po_last), po_raw = F9_00_PRIN_OFF_NAME_PERS)],
              lk[, .(OBJECTID, PERSON_YEAR_ID, EMP_ID, first = norm(first_name), last = norm(last_name),
                     role.final, org.leadership, ein, taxyr)], by = "OBJECTID", allow.cartesian = TRUE)
cand <- cand[nzchar(po_last) & last == po_last &
             (first == po_first | (nchar(first) == 1 | nchar(po_first) == 1) & substr(first, 1, 1) == substr(po_first, 1, 1))]
# one match per filing: prefer an exact first name, then a CEO
cand[, pri := (first == po_first) * 2 + (role.final == "CEO")]
setorder(cand, OBJECTID, -pri)
po <- cand[!duplicated(OBJECTID)]
put("B1", "principal officer found in Part VII", nrow(po) / nrow(f9), nrow(f9))
put("B2", "found principal officer is step 09 CEO", po[, mean(role.final == "CEO")], nrow(po))
for (r in setdiff(unique(po$role.final), "CEO"))
  put("B2", sprintf("found principal officer is %s", r), po[, mean(role.final == r)], nrow(po))
# In an organization with no paid staff the principal officer is usually the
# board president or treasurer, and BOARD is the right label. The CEO check
# only means something where step 09 found paid leadership.
for (ld in c("designated", "imputed", "board_governed", "no_paid"))
  put("B2", sprintf("principal officer is CEO | org.leadership = %s", ld),
      po[org.leadership == ld, mean(role.final == "CEO")], po[org.leadership == ld, .N])
put("B2", "filings with a CEO whose CEO is the principal officer",
    po[OBJECTID %in% lk[role.final == "CEO", OBJECTID], mean(role.final == "CEO")],
    po[OBJECTID %in% lk[role.final == "CEO", OBJECTID], .N])

pr <- merge(po[, .(ein, taxyr, EMP_ID, po_first, po_last)],
            po[, .(ein, taxyr = taxyr - 1L, EMP_ID1 = EMP_ID, po_first1 = po_first, po_last1 = po_last)],
            by = c("ein", "taxyr"))
same <- pr[po_last == po_last1 & po_first == po_first1]
diff <- pr[po_last != po_last1 & substr(po_first, 1, 1) != substr(po_first1, 1, 1)]
put("B3", "same principal officer next year -> same EMP_ID (recall)", same[, mean(EMP_ID == EMP_ID1)], nrow(same))
put("B3", "different principal officer next year -> same EMP_ID (false merge)", diff[, mean(EMP_ID == EMP_ID1)], nrow(diff))

# ---- C. board size --------------------------------------------------------------------
bd <- lk[formtype == "990", .(seats = sum(!is.na(role.board) & nzchar(role.board)),
                               board = sum(role.final == "BOARD"),
                               trustee_box = sum(trustee.x %in% c("1", "X", "TRUE", "true"))), by = OBJECTID]
bd <- merge(bd, fil[, .(OBJECTID, vote = num(F9_06_GVRN_NUM_VOTING_MEMB))], by = "OBJECTID")[!is.na(vote)]
for (v in c("seats", "board", "trustee_box")) {
  put("C", sprintf("%s = voting members", v), bd[, mean(get(v) == vote)], nrow(bd))
  put("C", sprintf("%s within 2 of voting members", v), bd[, mean(abs(get(v) - vote) <= 2)], nrow(bd))
  put("C", sprintf("%s median (count - voting members)", v), bd[, median(get(v) - vote)], nrow(bd))
}

# ---- D. CEO continuity ------------------------------------------------------------------
ceo <- lk[role.final == "CEO", .(EMP_ID = list(unique(EMP_ID))), by = .(ein, taxyr)]
cp <- merge(ceo, ceo[, .(ein, taxyr = taxyr - 1L, EMP_ID1 = EMP_ID)], by = c("ein", "taxyr"))
cp[, kept := mapply(function(a, b) length(intersect(a, b)) > 0, EMP_ID, EMP_ID1)]
put("D", "CEO is the same EMP_ID next year", cp[, mean(kept)], nrow(cp))

# ---- E. review flags --------------------------------------------------------------------
fl <- tryCatch(as.data.table(flag_links(as.data.frame(lk))), error = function(e) { message("flag_links: ", conditionMessage(e)); NULL })
if (!is.null(fl)) for (k in sort(unique(na.omit(fl$category))))
  put("E", paste("flag_links persons:", k), fl[category == k, .N], nrow(pers))
fa <- tryCatch(flag_anomalies(as.data.frame(lk)), error = function(e) { message("flag_anomalies: ", conditionMessage(e)); NULL })
if (!is.null(fa)) {
  q <- as.data.table(anomaly_review_queue(fa))
  for (k in sort(unique(q$category))) put("E", paste("flag_anomalies persons:", k), q[category == k, uniqueN(emp_id)], nrow(pers))
}

out <- rbindlist(M)
fwrite(out, file.path(sd_repo, sprintf("baseline-%s.csv", TAG)))
print(out, nrows = 200)
