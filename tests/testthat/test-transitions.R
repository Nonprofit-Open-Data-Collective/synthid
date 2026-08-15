## A hand-built *linked* panel (EMP_ID already stamped) so the analytics are
## tested in isolation from the matcher. One record per (ein, taxyr, EMP_ID).
rec <- function(ein, taxyr, emp, name, std, raw = std, comp = NA_real_) {
  data.frame(ein = ein, taxyr = taxyr, EMP_ID = emp, name = name,
             title.standard = std, title.raw = raw, tot.comp = comp,
             stringsAsFactors = FALSE)
}

# ---- position transitions ---------------------------------------------------

test_that("a new EMP_ID taking an occupied seat is flagged a transition", {
  df <- rbind(
    rec("A", 2018, "E-ALICE", "ALICE A", "EXECUTIVE DIRECTOR"),
    rec("A", 2019, "E-ALICE", "ALICE A", "EXECUTIVE DIRECTOR"),
    rec("A", 2020, "E-BOB",   "BOB B",   "EXECUTIVE DIRECTOR"),
    rec("A", 2021, "E-BOB",   "BOB B",   "EXECUTIVE DIRECTOR"),
    # different org, seat established with no prior holder -> incoming, not a transition
    rec("B", 2020, "E-CARL",  "CARL C",  "EXECUTIVE DIRECTOR")
  )
  tr <- flag_position_transitions(df, position = "EXECUTIVE DIRECTOR")

  expect_s3_class(tr, "synthid_transitions")
  # exactly one genuine handover: BOB replacing ALICE at A/2020
  hand <- tr[tr$transition, ]
  expect_equal(nrow(hand), 1L)
  expect_equal(hand$ein, "A")
  expect_equal(hand$taxyr, 2020L)
  expect_equal(hand$EMP_ID, "E-BOB")
  expect_equal(hand$predecessor, "E-ALICE")

  # first-ever holders are incoming but not transitions
  a18 <- tr[tr$ein == "A" & tr$taxyr == 2018, ]
  expect_true(a18$incoming)
  expect_false(a18$transition)
  b20 <- tr[tr$ein == "B", ]
  expect_true(b20$incoming)
  expect_false(b20$transition)   # org B has no prior filing year
})

test_that("gap_tolerant bridges a vacant seat-year; strict does not", {
  df <- rbind(
    rec("A", 2018, "E-ALICE", "ALICE A", "EXECUTIVE DIRECTOR"),
    rec("A", 2019, "E-ALICE", "ALICE A", "EXECUTIVE DIRECTOR"),
    # 2020: org files (a board member) but the ED seat is vacant
    rec("A", 2020, "E-BD",    "BD B",    "TRUSTEE"),
    rec("A", 2021, "E-BOB",   "BOB B",   "EXECUTIVE DIRECTOR")
  )
  strict <- flag_position_transitions(df, position = "EXECUTIVE DIRECTOR")
  b21s <- strict[strict$EMP_ID == "E-BOB", ]
  expect_true(b21s$incoming)
  expect_false(b21s$transition)          # 2020 filing year had no ED -> establishment

  gapt <- flag_position_transitions(df, position = "EXECUTIVE DIRECTOR",
                                    gap_tolerant = TRUE)
  b21g <- gapt[gapt$EMP_ID == "E-BOB", ]
  expect_true(b21g$transition)           # bridges the 2020 vacancy back to ALICE (2019)
  expect_equal(b21g$predecessor, "E-ALICE")
  expect_equal(b21g$prev_year, 2019L)
  expect_equal(b21g$years_since_prev, 2L)
})

test_that("selecting by role picks a whole governance body", {
  df <- rbind(
    rec("A", 2019, "E-P", "P P", "PRESIDENT"),
    rec("A", 2019, "E-Q", "Q Q", "TRUSTEE"),
    rec("A", 2019, "E-Z", "Z Z", "CHIEF EXECUTIVE OFFICER")   # OFFICER, excluded
  )
  tr <- flag_position_transitions(df, role = "BOARD")
  expect_setequal(tr$EMP_ID, c("E-P", "E-Q"))
})

# ---- churn ------------------------------------------------------------------

test_that("board churn counts new / left / stable against the prior year", {
  board <- function(ein, taxyr, emp) rec(ein, taxyr, emp, emp, "TRUSTEE")
  df <- rbind(
    board("A", 2018, "P"), board("A", 2018, "Q"), board("A", 2018, "R"),
    board("A", 2019, "P"), board("A", 2019, "Q"), board("A", 2019, "S"),
    board("A", 2020, "P"), board("A", 2020, "Q"), board("A", 2020, "S")
  )
  ch <- position_churn(df, role = "BOARD")
  expect_s3_class(ch, "synthid_churn")
  expect_equal(ch$taxyr, c(2018L, 2019L, 2020L))
  expect_equal(ch$n_current, c(3L, 3L, 3L))
  # first year: no prior -> NA
  expect_true(is.na(ch$n_new[1]) && is.na(ch$n_left[1]) && is.na(ch$n_stable[1]))
  # 2019: +S, -R, P/Q stable
  expect_equal(ch$n_new[2], 1L)
  expect_equal(ch$n_left[2], 1L)
  expect_equal(ch$n_stable[2], 2L)
  # 2020: no change
  expect_equal(ch$n_new[3], 0L)
  expect_equal(ch$n_left[3], 0L)
  expect_equal(ch$n_stable[3], 3L)
})

test_that("pooled churn drops the org column and aggregates the panel", {
  df <- rbind(
    rec("A", 2019, "E1", "n", "CEO"), rec("B", 2019, "E2", "n", "CEO"),
    rec("A", 2020, "E1", "n", "CEO"), rec("B", 2020, "E3", "n", "CEO")
  )
  ch <- position_churn(df, position = "CEO", by_org = FALSE)
  expect_false("ein" %in% names(ch))
  expect_equal(ch$n_current, c(2L, 2L))
  expect_equal(ch$n_new[2], 1L)    # E3 arrives
  expect_equal(ch$n_left[2], 1L)   # E2 departs
  expect_equal(ch$n_stable[2], 1L) # E1 stays
})

# ---- promotions -------------------------------------------------------------

test_that("a real role move with a pay jump is a promotion", {
  df <- rbind(
    rec("A", 2018, "E-X", "X X", "PROGRAM DIRECTOR",   comp = 60000),
    rec("A", 2019, "E-X", "X X", "EXECUTIVE DIRECTOR", comp = 95000),
    rec("A", 2020, "E-X", "X X", "EXECUTIVE DIRECTOR", comp = 96000)
  )
  pr <- flag_promotions(df)
  expect_s3_class(pr, "synthid_promotions")
  expect_equal(nrow(pr), 1L)                 # only the 2018->2019 change
  expect_equal(as.character(pr$change_type), "real")
  expect_equal(pr$direction, "promotion")
  expect_equal(pr$basis, "comp")
})

test_that("a pay drop with a role step-down is a demotion", {
  df <- rbind(
    rec("A", 2018, "E-Y", "Y Y", "EXECUTIVE DIRECTOR", comp = 90000),
    rec("A", 2019, "E-Y", "Y Y", "PROGRAM DIRECTOR",   comp = 40000)
  )
  pr <- flag_promotions(df)
  expect_equal(as.character(pr$change_type), "real")
  expect_equal(pr$direction, "demotion")
})

test_that("identical raw title marks a standardizer relabel, not a promotion", {
  df <- rbind(
    rec("A", 2018, "E-Z", "Z Z", "CHIEF EXECUTIVE OFFICER", raw = "CEO", comp = 1e5),
    rec("A", 2019, "E-Z", "Z Z", "EXECUTIVE DIRECTOR",      raw = "CEO", comp = 1e5)
  )
  pr <- flag_promotions(df)
  expect_equal(as.character(pr$change_type), "relabel")
  expect_true(is.na(pr$direction))
})

test_that("upstream repetition and one-year reverts are noise, not promotions", {
  df <- rbind(
    rec("A", 2018, "E-W", "W W", "MANAGER"),
    rec("A", 2019, "E-W", "W W", "DIRECTOR OF OPERATIONS"),
    rec("A", 2020, "E-W", "W W", "MANAGER")
  )
  pr <- flag_promotions(df)
  ct <- as.character(pr$change_type[order(pr$to_year)])
  # 2019: MANAGER->DIR OF OPS reverts next year -> transient
  # 2020: back to MANAGER, seen upstream in 2018 -> oscillation
  expect_equal(ct, c("transient", "oscillation"))
  expect_true(all(is.na(pr$direction)))
})

test_that("classed print falls back cleanly after column subsetting", {
  df <- rbind(
    rec("A", 2018, "E-ALICE", "ALICE A", "EXECUTIVE DIRECTOR"),
    rec("A", 2019, "E-BOB",   "BOB B",   "EXECUTIVE DIRECTOR")
  )
  tr <- flag_position_transitions(df, position = "EXECUTIVE DIRECTOR")
  sub <- tr[, c("taxyr", "EMP_ID", "transition")]   # drops the summary attribute
  expect_null(attr(sub, "synthid_transitions"))
  out <- paste(capture.output(print(sub)), collapse = "\n")
  expect_false(grepl("synthid position transitions", out))  # no misleading 0-summary
  expect_true(grepl("E-BOB", out))                          # rows still print
})

test_that("functions demand a linked panel", {
  df <- rec("A", 2019, NA, "n", "CEO"); df$EMP_ID <- NULL
  expect_error(flag_promotions(df), "Run link_panel")
  expect_error(position_churn(df, position = "CEO"), "Run link_panel")
  expect_error(flag_position_transitions(df, position = "CEO"), "Run link_panel")
})
