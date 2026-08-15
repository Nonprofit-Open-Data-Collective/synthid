## Phase-2: match_to_profiles() -- attach a new wave of people to a frozen
## existing person set. Builds tiny panels, links each, profiles them, matches.

rec_k <- function(ein, yr, first, last, suffix, gender, title, oid, tid) {
  data.frame(
    ein = ein, taxyr = yr,
    name = trimws(paste(first, last, suffix)),
    salutation = NA_character_, first_name = first, middle_name = NA_character_,
    last_name = last, suffix = suffix, gender = gender, title.standard = title,
    OBJECTID = oid, TABLE_ID = tid, stringsAsFactors = FALSE
  )
}

profiles_of <- function(raw) build_person_profile(link_panel(raw))

test_that("a returning person inherits the existing EMP_ID and anchor; a new one does not", {
  existing_raw <- rbind(
    rec_k("100", 2019, "JOHN", "SMITH", "SR", "M", "CEO", "OE1", "T1"),
    rec_k("100", 2020, "JOHN", "SMITH", "SR", "M", "CEO", "OE2", "T1"),
    rec_k("100", 2021, "JOHN", "SMITH", "SR", "M", "CEO", "OE3", "T1"),
    rec_k("100", 2019, "ANN",  "JONES", "",   "F", "SECRETARY", "OE4", "T2")
  )
  wave_raw <- rbind(
    rec_k("100", 2022, "JOHN", "SMITH", "SR", "M", "CEO", "OW1", "T1"),        # returning
    rec_k("100", 2022, "BARB", "NGUYEN", "",  "F", "TREASURER", "OW2", "T2")   # brand new
  )
  ex <- profiles_of(existing_raw); wv <- profiles_of(wave_raw)
  sr_existing_id <- ex$EMP_ID[ex$last_name == "SMITH"]
  sr_existing_anchor <- ex$EMP_ANCHOR[ex$last_name == "SMITH"]

  res <- match_to_profiles(wv, ex)

  # John Smith Sr matched to the existing Sr, inheriting id + anchor.
  expect_equal(nrow(res$matched), 1L)
  expect_identical(res$matched$existing_emp_id, sr_existing_id)
  expect_identical(res$matched$existing_emp_anchor, sr_existing_anchor)
  # Barb Nguyen is a new person -> unmatched (to be minted fresh downstream).
  barb_id <- wv$EMP_ID[wv$last_name == "NGUYEN"]
  expect_true(barb_id %in% res$unmatched)
  expect_equal(res$report$n_new_persons, 1L)
})

test_that("an org-year collision is rejected as a different person, not matched", {
  existing_raw <- rbind(
    rec_k("100", 2019, "JOHN", "SMITH", "SR", "M", "CEO", "OE1", "T1"),
    rec_k("100", 2020, "JOHN", "SMITH", "SR", "M", "CEO", "OE2", "T1"),
    rec_k("100", 2021, "JOHN", "SMITH", "SR", "M", "CEO", "OE3", "T1")
  )
  # wave re-files a 2020 John Smith -- a year the existing Sr already occupies.
  wave_raw <- rec_k("100", 2020, "JOHN", "SMITH", "SR", "M", "CEO", "OW9", "T1")
  ex <- profiles_of(existing_raw); wv <- profiles_of(wave_raw)

  res <- match_to_profiles(wv, ex)

  expect_equal(nrow(res$matched), 0L)                 # cannot be the same person
  expect_equal(res$report$n_invariant_collisions, 1L)
  expect_true("invariant_collision" %in% res$review$reason)
  expect_true(wv$EMP_ID %in% res$unmatched)
})

test_that("one wave person matching two existing people yields an ambiguous review row", {
  # Two similar existing people in org 100 (same soundex S530), distinct years so
  # neither collides with the wave; one wave John Smith matches both.
  existing_raw <- rbind(
    rec_k("100", 2019, "JOHN", "SMITH",  "", "M", "CEO",      "OE1", "T1"),
    rec_k("100", 2019, "JOHN", "SMYTHE", "", "M", "DIRECTOR", "OE2", "T2")
  )
  wave_raw <- rec_k("100", 2022, "JOHN", "SMITH", "", "M", "CEO", "OW1", "T1")
  ex <- profiles_of(existing_raw); wv <- profiles_of(wave_raw)

  # Low threshold so both existing candidates clear it and contend for the match.
  res <- match_to_profiles(wv, ex, threshold = 3)

  expect_equal(nrow(res$matched), 1L)                 # only one can win
  expect_true("ambiguous" %in% res$review$reason)
  expect_gte(res$report$n_ambiguous, 1L)
})

test_that("no shared org -> no candidates, everyone unmatched", {
  ex <- profiles_of(rec_k("100", 2019, "JOHN", "SMITH", "", "M", "CEO", "OE1", "T1"))
  wv <- profiles_of(rec_k("200", 2022, "JOHN", "SMITH", "", "M", "CEO", "OW1", "T1"))
  res <- match_to_profiles(wv, ex)
  expect_equal(nrow(res$matched), 0L)
  expect_equal(res$report$n_candidate_pairs, 0L)
  expect_setequal(res$unmatched, wv$EMP_ID)
})

test_that("overlapping wave/existing EMP_IDs are rejected", {
  ex <- profiles_of(rec_k("100", 2019, "JOHN", "SMITH", "", "M", "CEO", "OE1", "T1"))
  expect_error(match_to_profiles(ex, ex), "overlap")
})

## ---- link_incremental(): end-to-end wave integration -----------------------

test_that("link_incremental stamps returning people with existing ids and new ones fresh", {
  existing <- link_panel(rbind(
    rec_k("100", 2019, "JOHN", "SMITH", "SR", "M", "CEO", "OE1", "T1"),
    rec_k("100", 2020, "JOHN", "SMITH", "SR", "M", "CEO", "OE2", "T1"),
    rec_k("100", 2019, "ANN",  "JONES", "",   "F", "SECRETARY", "OE3", "T2")
  ))
  sr_id <- unique(existing$EMP_ID[existing$suffix == "SR"])

  wave <- rbind(
    rec_k("100", 2021, "JOHN", "SMITH",  "SR", "M", "CEO", "OW1", "T1"),        # returning
    rec_k("100", 2021, "DIANE", "OKAFOR", "",  "F", "TREASURER", "OW2", "T2")   # new
  )
  res <- link_incremental(existing, wave)

  expect_equal(nrow(res$new_stamped), 2L)
  sr_row  <- res$new_stamped[res$new_stamped$last_name == "SMITH", ]
  new_row <- res$new_stamped[res$new_stamped$last_name == "OKAFOR", ]
  expect_identical(sr_row$EMP_ID, sr_id)                 # inherited
  expect_false(new_row$EMP_ID %in% existing$EMP_ID)      # genuinely new id
  expect_true(new_row$EMP_ID %in% res$unmatched)
  expect_equal(res$report$n_rows_returning, 1L)
  expect_equal(res$report$n_rows_first_time, 1L)

  # after merge, Sr is ONE person across 2019-2021
  merged <- rbind(existing[names(res$new_stamped)], res$new_stamped)
  expect_equal(sort(unique(merged$taxyr[merged$EMP_ID == sr_id])), c(2019, 2020, 2021))
})

test_that("backfilling an earlier year freezes the existing anchor (does NOT re-anchor)", {
  existing <- link_panel(rbind(
    rec_k("100", 2019, "ANN", "JONES", "", "F", "SECRETARY", "OE1", "T1"),
    rec_k("100", 2021, "ANN", "JONES", "", "F", "SECRETARY", "OE2", "T1")
  ))
  ann_id0     <- unique(existing$EMP_ID)
  ann_anchor0 <- unique(existing$EMP_ANCHOR)          # anchored on the 2019 record

  # wave backfills a 2018 Ann Jones -- earlier than the current 2019 anchor.
  wave <- rec_k("100", 2018, "ANN", "JONES", "", "F", "SECRETARY", "OW1", "T9")
  res <- link_incremental(existing, wave)

  expect_equal(nrow(res$new_stamped), 1L)
  # The backfill inherits Ann's existing id AND her frozen 2019 anchor -- the 2018
  # row must NOT become a new anchor (the property a batch rebuild cannot give).
  expect_identical(res$new_stamped$EMP_ID, ann_id0)
  expect_identical(res$new_stamped$EMP_ANCHOR, ann_anchor0)
  expect_false(res$new_stamped$EMP_ANCHOR == person_year_id("OW1", "T9"))
})

test_that("a multi-year wave links a first-time person across its own years first", {
  existing <- link_panel(
    rec_k("100", 2019, "JOHN", "SMITH", "", "M", "CEO", "OE1", "T1")
  )
  # brand-new person appears in TWO wave years; must collapse to one person/id.
  wave <- rbind(
    rec_k("100", 2020, "CARLOS", "REYES", "", "M", "DIRECTOR", "OW1", "T5"),
    rec_k("100", 2021, "CARLOS", "REYES", "", "M", "DIRECTOR", "OW2", "T5")
  )
  res <- link_incremental(existing, wave)

  reyes <- res$new_stamped[res$new_stamped$last_name == "REYES", ]
  expect_equal(nrow(reyes), 2L)
  expect_equal(length(unique(reyes$EMP_ID)), 1L)          # one person, not two
  expect_false(unique(reyes$EMP_ID) %in% existing$EMP_ID) # new
  expect_equal(res$report$n_new_persons, 1L)
})

test_that("link_incremental requires an anchored (EMP_ANCHOR) existing panel", {
  existing <- link_panel(rec_k("100", 2019, "JOHN", "SMITH", "", "M", "CEO", "OE1", "T1"))
  existing$EMP_ANCHOR <- NULL
  wave <- rec_k("100", 2020, "JOHN", "SMITH", "", "M", "CEO", "OW1", "T1")
  expect_error(link_incremental(existing, wave), "EMP_ANCHOR")
})

test_that("exact re-loads (shared OBJECTID/TABLE_ID) are dropped with a warning", {
  existing <- link_panel(rbind(
    rec_k("100", 2019, "JOHN", "SMITH", "", "M", "CEO", "OE1", "T1"),
    rec_k("100", 2020, "JOHN", "SMITH", "", "M", "CEO", "OE2", "T1")
  ))
  # wave carries one genuinely-new row plus a re-load of the existing (OE2,T1) row.
  wave <- rbind(
    rec_k("100", 2021, "JOHN", "SMITH", "", "M", "CEO", "OW1", "T1"),  # new year
    rec_k("100", 2020, "JOHN", "SMITH", "", "M", "CEO", "OE2", "T1")   # re-load
  )
  expect_warning(res <- link_incremental(existing, wave), "re-load")
  expect_equal(res$report$n_reload_rows_dropped, 1L)
  expect_equal(nrow(res$new_stamped), 1L)                 # only the new-year row
  expect_equal(res$new_stamped$taxyr, 2021)
})
