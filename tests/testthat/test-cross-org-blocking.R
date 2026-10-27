## Cross-org blocking: the relaxation ladder, and the metro pass that catches an
## interlock across a metro area spanning a state line.

## A linked panel is build_person_profile()'s input, and link_panel() returns its
## input plus the EMP_* columns, so a profile's blocking components ride along as
## ordinary columns. These rows stand in for that output.
rec_b <- function(ein, emp, first, last, state, cbsa, ntee) {
  data.frame(
    ein = ein, taxyr = 2020L, name = trimws(paste(first, last)),
    first_name = first, last_name = last, title.standard = "BOARD MEMBER",
    org.state = state, org.cbsa = cbsa, org.ntee = ntee,
    EMP_ID = emp, stringsAsFactors = FALSE
  )
}

# Four people named SMITH. NY_A and NJ_B are in the New York-Newark CBSA on
# either side of the state line; NY_A and NY_C share a state; CA_D shares
# neither state, metro nor sector.
panel_b <- function() {
  rbind(
    rec_b("EIN-1", "NY_A", "JANE", "SMITH", "NY", "35620", "B25"),
    rec_b("EIN-2", "NJ_B", "JANE", "SMITH", "NJ", "35620", "B25"),
    rec_b("EIN-3", "NY_C", "JANE", "SMITH", "NY", "10580", "B25"),
    rec_b("EIN-4", "CA_D", "JANE", "SMITH", "CA", "41860", "E20")
  )
}

prof_b <- function(...) {
  build_person_profile(panel_b(), state = "org.state", cbsa = "org.cbsa",
                       ntee = "org.ntee", ...)
}

test_that("the ladder is ordered tightest first and metro sits below strict", {
  p <- blocking_passes()
  expect_identical(names(p), c("strict", "metro", "geo", "industry", "surname"))
  expect_identical(p$metro, c("last", "cbsa", "ntee"))
  # surname is in every pass, so a pair can only form within one surname key.
  expect_true(all(vapply(p, function(k) "last" %in% k, logical(1))))
})

test_that("a cbsa column becomes a per-person variant set", {
  prof <- prof_b()
  expect_true("cbsa" %in% names(prof))
  cb <- setNames(prof$cbsa, prof$EMP_ID)
  expect_identical(cb[["NY_A"]], "35620")
  expect_identical(cb[["CA_D"]], "41860")
})

test_that("the metro pass emits keys on the cbsa, in the order the pass declares", {
  keys <- person_blocking_keys(prof_b())
  expect_setequal(unique(keys$pass), names(blocking_passes()))
  mk <- keys$block_key[keys$pass == "metro"]
  # metro|<surname key>|<cbsa>|<ntee major group>
  expect_true(all(grepl("^metro\\|[A-Z][0-9]+\\|[0-9]{5}\\|[A-Z]$", mk)))
  expect_true("metro|S530|35620|B" %in% mk)
})

test_that("metro catches the two-state metro pair that strict cannot see", {
  prof <- prof_b()
  cand <- candidate_pairs(person_blocking_keys(prof), prof)
  pass_of <- function(a, b) {
    i <- (cand$emp_a == a & cand$emp_b == b) | (cand$emp_a == b & cand$emp_b == a)
    if (!any(i)) NA_character_ else cand$pass[i][1]
  }
  # same state and sector -> strict; same metro across the state line -> metro,
  # which is tighter than the geo pass it would otherwise fall through to.
  expect_identical(pass_of("NY_A", "NY_C"), "strict")
  expect_identical(pass_of("NY_A", "NJ_B"), "metro")
  expect_identical(pass_of("CA_D", "NY_A"), "surname")
})

test_that("without a cbsa column the metro pass emits nothing and the rest are unchanged", {
  prof <- build_person_profile(panel_b(), state = "org.state", ntee = "org.ntee")
  expect_false("cbsa" %in% names(prof))
  keys <- person_blocking_keys(prof)
  expect_false("metro" %in% keys$pass)
  expect_setequal(unique(keys$pass), c("strict", "geo", "industry", "surname"))
  # the pair that metro would have caught falls through to geo
  cand <- candidate_pairs(keys, prof)
  i <- (cand$emp_a == "NJ_B" & cand$emp_b == "NY_A") |
       (cand$emp_a == "NY_A" & cand$emp_b == "NJ_B")
  expect_identical(cand$pass[i][1], "geo")
})

test_that("a person filing from two metros lands in both metro buckets", {
  two <- rbind(
    rec_b("EIN-1", "MOVER", "JANE", "SMITH", "NY", "35620", "B25"),
    rec_b("EIN-2", "MOVER", "JANE", "SMITH", "CA", "41860", "B25")
  )
  prof <- build_person_profile(two, state = "org.state", cbsa = "org.cbsa",
                               ntee = "org.ntee")
  expect_identical(prof$cbsa[[1]], c("35620", "41860"))
  mk <- person_blocking_keys(prof)
  mk <- mk$block_key[mk$pass == "metro"]
  expect_setequal(mk, c("metro|S530|35620|B", "metro|S530|41860|B"))
})

test_that("a compound surname blocks with each of its components across orgs", {
  panel <- rbind(
    rec_b("EIN-1", "COMPOUND", "MARIA", "GARCIA-LOPEZ", "NY", "35620", "B25"),
    rec_b("EIN-2", "FIRST",    "MARIA", "GARCIA",       "NY", "35620", "B25"),
    rec_b("EIN-3", "SECOND",   "MARIA", "LOPEZ",        "NY", "35620", "B25")
  )
  prof <- build_person_profile(panel, state = "org.state", cbsa = "org.cbsa",
                               ntee = "org.ntee")
  keys <- setNames(prof$last_name_keys, prof$EMP_ID)
  # the whole-string key is shared with neither component: that is why the
  # compound surname was invisible to blocking before the component keys
  expect_false(any(c("G620", "L120") == "G624"))
  expect_true("G624" %in% keys[["COMPOUND"]])
  expect_true(all(c("G620", "L120") %in% keys[["COMPOUND"]]))

  cand <- candidate_pairs(person_blocking_keys(prof), prof)
  pairs <- paste(cand$emp_a, cand$emp_b)
  expect_true(any(pairs %in% c("COMPOUND FIRST", "FIRST COMPOUND")))
  expect_true(any(pairs %in% c("COMPOUND SECOND", "SECOND COMPOUND")))
  # the two single-surname people share no key, so they are not a candidate
  expect_false(any(pairs %in% c("FIRST SECOND", "SECOND FIRST")))
})
