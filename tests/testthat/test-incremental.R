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
