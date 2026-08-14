test_that("exact and nickname matches score high", {
  expect_equal(compare_first_names("JOHN", "JOHN"), 1)
  expect_equal(compare_first_names("bob", "robert"), 0.95)   # case-insensitive
  expect_equal(compare_first_names("BILL", "WILLIAM"), 0.95)
  expect_equal(compare_first_names("STEVE", "STEPHEN"), 0.95) # STEVE is a variant of STEPHEN
  expect_equal(compare_first_names("SUE", "SUSAN"), 0.95)
})

test_that("genuinely different first names are penalised (score toward 0)", {
  expect_lt(compare_first_names("ALEXIS", "MARCELLA"), 0.3)
  expect_lt(compare_first_names("SARAH", "MICHAEL"), 0.3)
  # this is the point: plain JW would sit near 0.5 (neutral); rescaling pushes it down
  expect_lt(compare_first_names("ALEXIS", "MARCELLA"),
            1 - stringdist::stringdist("ALEXIS", "MARCELLA", method = "jw"))
})

test_that("initials are weak evidence, not strong matches", {
  expect_equal(compare_first_names("J", "JOHN"), 0.6)
  expect_equal(compare_first_names("J", "MARY"), 0.0)
})

test_that("phonetic agreement removes the penalty without asserting a match", {
  s <- compare_first_names("STEPHEN", "STEVEN")  # same Soundex, not in nickname table together
  expect_gt(s, 0.5)
  expect_lt(s, 0.9)
})

test_that("missing names return NA and the comparator is vectorised", {
  s <- compare_first_names(c("BOB", NA, "J"), c("ROBERT", "ANN", "JOHN"))
  expect_length(s, 3)
  expect_equal(s[1], 0.95)
  expect_true(is.na(s[2]))
  expect_equal(s[3], 0.6)
})

test_that("only DIRECT nickname pairs are equivalent, not siblings", {
  expect_true("ROBERT" %in% first_name_roots("BOB"))
  expect_true(nickname_equivalent_vec("JIM", "JAMES"))   # variant <-> canonical
  expect_true(nickname_equivalent_vec("BOB", "ROBERT"))
  expect_false(nickname_equivalent_vec("JIM", "JOHN"))
  # LISA and BETH are both diminutives of ELIZABETH but denote different people
  expect_false(nickname_equivalent_vec("LISA", "BETH"))
  expect_lt(compare_first_names("LISA", "BETH"), 0.6)
})
