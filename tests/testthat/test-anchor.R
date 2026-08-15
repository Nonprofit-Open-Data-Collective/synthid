## Phase-1 anchored-id guarantees: an EMP_ID is minted from the person's
## first-seen record, so it survives new waves (later records) and backfills
## (earlier records), and is independent of row order and run.

test_that("emp_anchor_key picks the earliest-year record's PYID", {
  key <- emp_anchor_key(
    year      = c(2021, 2019, 2020),
    object_id = c("O3", "O1", "O2"),
    table_id  = c("T3", "T1", "T2"),
    row_uid   = c("r3", "r1", "r2")
  )
  expect_identical(key, person_year_id("O1", "T1"))
})

test_that("emp_anchor_key is order-independent", {
  a <- emp_anchor_key(c(2019, 2020), c("O1", "O2"), c("T1", "T2"), c("r1", "r2"))
  b <- emp_anchor_key(c(2020, 2019), c("O2", "O1"), c("T2", "T1"), c("r2", "r1"))
  expect_identical(a, b)
})

test_that("emp_anchor_key falls back to row_uid without native keys", {
  key <- emp_anchor_key(year = c(2020, 2019), row_uid = c("r-late", "r-early"))
  expect_identical(key, "r-early")
})

test_that("emp_anchor_key prefers a record carrying a native key within a tie year", {
  # both records are 2019; only the second has OBJECTID/TABLE_ID -> anchor there
  key <- emp_anchor_key(
    year      = c(2019, 2019),
    object_id = c(NA, "O9"),
    table_id  = c(NA, "T9"),
    row_uid   = c("r-a", "r-b")
  )
  expect_identical(key, person_year_id("O9", "T9"))
})

test_that("anchor_emp_id is deterministic, prefixed, and off the legacy keyspace", {
  a <- anchor_emp_id(c("PYID-x", "PYID-y", "PYID-x"))
  expect_equal(a[1], a[3])
  expect_false(a[1] == a[2])
  expect_true(all(grepl("^EMP-", a)))
  expect_equal(anchor_emp_id("PYID-x"), anchor_emp_id("PYID-x"))
  # different scheme tag than the legacy hash-of-membership id for the same string
  expect_false(anchor_emp_id("PYID-x") == create_emp_ids("PYID-x"))
})

## ---- end-to-end stability through link_panel -------------------------------

# helper-make-panel.R has no native keys; add OBJECTID/TABLE_ID so the anchor
# path exercises PYIDs, and give each record a unique key.
panel_with_keys <- function() {
  p <- make_panel()
  p$OBJECTID <- sprintf("O%03d", seq_len(nrow(p)))
  p$TABLE_ID <- sprintf("T%03d", seq_len(nrow(p)))
  p
}

test_that("link_panel stamps EMP_ANCHOR and stays deterministic", {
  p <- panel_with_keys()
  a <- link_panel(p)
  b <- link_panel(p)
  expect_true("EMP_ANCHOR" %in% names(a))
  expect_equal(a$EMP_ID, b$EMP_ID)
  expect_equal(a$EMP_ANCHOR, b$EMP_ANCHOR)
  # one anchor per person cluster
  expect_equal(length(unique(a$EMP_ANCHOR)), length(unique(a$EMP_ID)))
})

test_that("adding a later wave does not change an existing person's EMP_ID", {
  base <- panel_with_keys()
  linked0 <- link_panel(base)

  # a new 2022 wave: Sr and Jr in org 100 continue; a brand-new person appears
  wave <- rbind(
    data.frame(ein = "100", taxyr = 2022, name = "JOHN SMITH SR",
               salutation = NA, first_name = "JOHN", middle_name = NA,
               last_name = "SMITH", suffix = "SR", gender = "M",
               title.standard = "CEO", OBJECTID = "O201", TABLE_ID = "T201",
               stringsAsFactors = FALSE),
    data.frame(ein = "100", taxyr = 2022, name = "JOHN SMITH JR",
               salutation = NA, first_name = "JOHN", middle_name = NA,
               last_name = "SMITH", suffix = "JR", gender = "M",
               title.standard = "TREASURER", OBJECTID = "O202", TABLE_ID = "T202",
               stringsAsFactors = FALSE)
  )
  linked1 <- link_panel(rbind(base, wave))

  # Sr's id from the base run must equal Sr's id in the grown run (rows 1,4,7 = Sr)
  sr_before <- unique(linked0$EMP_ID[c(1, 4, 7)])
  sr_after  <- unique(linked1$EMP_ID[linked1$suffix == "SR" & linked1$ein == "100"])
  expect_length(sr_before, 1L)
  expect_length(sr_after, 1L)
  expect_identical(sr_before, sr_after)
})

# Contract boundary: a from-scratch link_panel() rebuild has no memory of when a
# person was "first seen", so backfilling an *earlier* record correctly re-picks
# the deterministic earliest as the anchor. Freezing the anchor across a backfill
# is a property of the incremental path (link_incremental, Phase 3), which carries
# the prior EMP_ANCHOR forward -- not of the batch builder.
test_that("a batch rebuild re-anchors on backfill (freeze is deferred to link_incremental)", {
  base <- panel_with_keys()
  linked0 <- link_panel(base)
  ann_id0 <- unique(linked0$EMP_ID[linked0$last_name == "JONES"])
  expect_length(ann_id0, 1L)

  # Backfill a *2018* Ann Jones row (earlier than her current 2019 anchor).
  backfill <- data.frame(
    ein = "100", taxyr = 2018, name = "ANN JONES",
    salutation = NA, first_name = "ANN", middle_name = NA,
    last_name = "JONES", suffix = "", gender = "F",
    title.standard = "SECRETARY", OBJECTID = "O300", TABLE_ID = "T300",
    stringsAsFactors = FALSE
  )
  linked1 <- link_panel(rbind(base, backfill))
  ann_id1 <- unique(linked1$EMP_ID[linked1$last_name == "JONES"])
  ann_anchor1 <- unique(linked1$EMP_ANCHOR[linked1$last_name == "JONES"])

  expect_length(ann_id1, 1L)
  # The 2018 row is now the deterministic earliest, so it becomes the anchor.
  expect_identical(ann_anchor1, person_year_id("O300", "T300"))
  expect_identical(ann_id1, anchor_emp_id(person_year_id("O300", "T300")))
  expect_false(ann_id1 == ann_id0)   # id moved, as expected for a batch rebuild
})

test_that("remint_anchored reproduces a fresh anchored link_panel run", {
  p <- panel_with_keys()
  fresh <- link_panel(p)

  # Simulate a legacy panel: same clustering, but overwrite EMP_ID with an
  # arbitrary legacy-style label per cluster and drop EMP_ANCHOR.
  legacy <- fresh
  legacy$EMP_ANCHOR <- NULL
  legacy$EMP_ID <- paste0("LEGACY-", match(fresh$EMP_ID, unique(fresh$EMP_ID)))

  out <- remint_anchored(legacy)
  # Re-minted ids must match the fresh anchored run exactly.
  expect_equal(out$panel$EMP_ID, fresh$EMP_ID)
  expect_equal(out$panel$EMP_ANCHOR, fresh$EMP_ANCHOR)
  # crosswalk covers every legacy id, one row each
  expect_setequal(out$crosswalk$old_emp_id, unique(legacy$EMP_ID))
  expect_equal(nrow(out$crosswalk), length(unique(legacy$EMP_ID)))
})
