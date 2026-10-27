# 08-hard-case-library.R
# Grow the hard-case library across a representative set of geography units,
# using the national cross-org run (07-xorg-csa.R). Run once: the library is a
# lasting artifact for calibrating the matcher.
#
# Units (CSA code, cbsa<code> for a CBSA outside any CSA, st<XX> for a
# state's organizations outside any CBSA), chosen for name patterns:
#   Midwest            378 Minneapolis-St. Paul (Scandinavian), 376 Milwaukee (German),
#                      stND rural North Dakota (German, Scandinavian), 176 Chicago (Polish, Midwest metro),
#                      220 Detroit (Arabic: AL-/EL-, MOHAMMED/MUHAMMAD; Chaldean; Polish)
#   South              406 New Orleans (French/Creole/Cajun particles and apostrophes; Vietnamese),
#                      298 Jackson MS and stMS rural Mississippi (Black Southern first-name spellings,
#                      JR/SR/III, small-town homonyms), 288 Houston (Vietnamese, Nigerian, Hispanic),
#                      122 Atlanta (Korean; Black professional networks),
#                      stWV, stKY rural Appalachia (surnames concentrated in small areas)
#   Hispanic           370 Miami, 348 Los Angeles, 365 McAllen (large pools of common Hispanic names),
#                      stTX rural Texas, 490 San Juan PR (two surnames as the norm)
#   West / Mountain    488 San Jose-San Francisco, 500 Seattle, 440 Portland (East and Southeast Asian),
#                      105 Albuquerque, 429 Phoenix, stAZ rural Arizona (Hispanic, Navajo and other
#                      Native names), 216 Denver, 482 Salt Lake City (JR/II/III; family names repeated
#                      within one organization)
#   Native nations     stSD rural South Dakota, stOK rural Oklahoma (translated multi-word surnames)
#   Pacific            cbsa46520 Urban Honolulu (Hawaiian, Japanese, Filipino, Chinese),
#                      cbsa11260 Anchorage, stAK rural Alaska (Alaska Native names)
#   Northeast          148 Boston (Irish O'/MC/MAC, Italian, Portuguese and Cape Verdean),
#                      428 Philadelphia, 408 New York (very large homonym pools)
# (450, Raleigh-Durham-Cary, is already in the library.)
#
# Per unit, each step in its own R process, skipping steps already done:
#   06a-unit-extract   profiles and scored pairs for the unit
#   06m-record-checks  record-level checks (and the panel-row cache)
#   06l-name-changes   surname changes
#   06k-name-checks    surname / first-name / suffix / middle-initial checks
#   06i-variant-test   matches resting on a pooled within-org variant
# NPAR units run at once (default 2), smallest first so the largest metros come
# last. A failed step is logged and the unit moves on (a failed extract skips
# the unit). Re-running picks up where it stopped. Then a count of the library
# by unit and category (no names) goes to dev/partvii-panel/hard-cases-summary.csv.
#
#   Rscript dev/partvii-panel/08-hard-case-library.R [units, comma-separated] [npar=2]

source("dev/partvii-panel/_panel.R")
source("dev/partvii-panel/_hard_cases.R")
build_step("08-hard-case-library")
args <- commandArgs(trailingOnly = TRUE)
UNITS <- if (length(args) >= 1 && nzchar(args[1])) strsplit(args[1], ",")[[1]] else c(
  # small and rural first, the largest metros last
  "stND", "stSD", "cbsa11260", "stAK", "stWV", "stKY", "stOK", "stAZ", "stMS", "298", "365", "cbsa46520",
  "490", "105", "376", "406", "482", "440", "stTX", "378", "216", "429", "500", "220", "122", "288",
  "370", "148", "428", "488", "176", "348", "408")
NPAR <- if (length(args) >= 2) as.integer(args[2]) else 2L
RSCRIPT <- file.path(R.home("bin"), "Rscript.exe")
if (!file.exists(file.path(pv_root, "xorg-csa", "pairs.parquet")))
  stop("the national run has not finished: no xorg-csa/pairs.parquet yet")
logs <- file.path(pv_root, "blocks", "hard-case-logs"); dir.create(logs, showWarnings = FALSE)
STEPS <- c("06a-unit-extract", "06m-record-checks", "06l-name-changes", "06k-name-checks", "06i-variant-test")

run_unit <- function(u, pv_root, pv_repo, logs, RSCRIPT, STEPS, extract_workers) {
  # Done = the summary table each script always writes (a library file may never
  # appear when a unit has no case of that kind).
  done <- function(step) { f <- file.path(pv_repo, paste0("csa-test-", u)); switch(step,
    "06a-unit-extract"  = file.path(pv_root, "blocks", "csa-test", paste0("csa=", u), "pairs.parquet"),
    "06m-record-checks" = file.path(f, "record-checks.csv"),
    "06l-name-changes"  = file.path(f, "name-changes-estimate.csv"),
    "06k-name-checks"   = file.path(f, "name-checks-gates.csv"),
    "06i-variant-test"  = file.path(f, "variant-test.csv")) }
  say <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", sprintf(...), "\n",
                           file = file.path(logs, "progress.log"), append = TRUE)
  out <- list()
  for (s in STEPS) {
    if (file.exists(done(s))) { say("%s %s: already done", u, s); next }
    t0 <- Sys.time(); say("%s %s: running", u, s)
    a <- c(file.path("dev", "partvii-panel", paste0(s, ".R")), u)
    if (s == "06a-unit-extract") a <- c(a, extract_workers)
    lf <- file.path(logs, sprintf("%s-%s.log", u, s))
    rc <- system2(RSCRIPT, a, stdout = lf, stderr = lf)
    m <- round(as.numeric(Sys.time() - t0, units = "mins"), 1)
    say("%s %s: %s in %.1f min", u, s, if (rc == 0) "done" else "FAILED (see log)", m)
    out[[length(out) + 1]] <- data.frame(unit = u, step = s, ok = rc == 0, minutes = m)
    if (rc != 0 && s == "06a-unit-extract") break    # no extract: nothing else for this unit can run
  }
  do.call(rbind, out)
}

# Extract every unit first in one shared pass over the profile files (one pass
# instead of one per unit), then run the checks NPAR units at a time.
stamp("extracting %d units", length(UNITS))
rc <- system2(RSCRIPT, c(file.path("dev", "partvii-panel", "06a-unit-extract.R"), paste(UNITS, collapse = ","), 16L),
              stdout = file.path(logs, "extract-all.log"), stderr = file.path(logs, "extract-all.log"))
if (rc != 0) stamp("the shared extract failed (see %s); units without an extract will try their own", file.path(logs, "extract-all.log"))
stamp("%d units, %d at a time; progress in %s", length(UNITS), NPAR, file.path(logs, "progress.log"))
cl <- parallel::makePSOCKcluster(NPAR)
parallel::clusterEvalQ(cl, setwd(getwd()))
st <- parallel::clusterApplyLB(cl, UNITS, run_unit, pv_root = pv_root, pv_repo = pv_repo, logs = logs,
                               RSCRIPT = RSCRIPT, STEPS = STEPS, extract_workers = if (NPAR > 1) 8L else 12L)
parallel::stopCluster(cl)
st <- rbindlist(lapply(st, as.data.table), fill = TRUE)
print(st)
fwrite(st, file.path(logs, "run-status.csv"))
if (nrow(st) && any(!st$ok)) stamp("%d step(s) failed: see %s", sum(!st$ok), logs)

h <- hard_cases_read()
summ <- dcast(h[, .N, by = .(csa, category)], category ~ csa, value.var = "N", fill = 0)
fwrite(summ, file.path(pv_repo, "hard-cases-summary.csv"))
print(summ)
stamp("library: %s cases across %d units", format(nrow(h), big.mark = ","), uniqueN(h$csa))
