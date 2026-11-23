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
  s <- compare_first_names("ROBERT", "RUPERT")  # same Soundex, not a listed pair
  expect_gt(s, 0.5)
  expect_lt(s, 0.9)
  local({ old <- options(synthid.first_name_lexicon = FALSE); on.exit(options(old))
    s <- compare_first_names("STEPHEN", "STEVEN")  # not a nickname_table() pair
    expect_gt(s, 0.5)
    expect_lt(s, 0.9)
  })
})

test_that("the lexicon adds pairs, scored by type", {
  lx <- first_name_lexicon()
  expect_gt(nrow(lx), 900)
  expect_setequal(unique(lx$type), c("nickname", "spelling", "compound", "cognate"))
  expect_equal(compare_first_names("STEPHEN", "STEVEN"), 0.95)   # spelling
  expect_equal(compare_first_names("KATHLEEN", "KATE"), 0.95)    # nickname not in nickname_table()
  expect_equal(compare_first_names("JEFFREY", "JEFF"), 0.95)
  expect_equal(compare_first_names("MARYANN", "MARY"), 0.90)     # compound
  expect_gte(compare_first_names("JOSEPH", "JOSE"), 0.85)        # cognate (or its fuzzy score, if higher)
  expect_lt(compare_first_names("JOSEPH", "JOSE"), 0.95)
  # pairs the review called different names are not listed
  expect_lt(compare_first_names("JOHN", "JOSH"), 0.85)
  expect_lt(compare_first_names("PAUL", "PAULA"), 0.95)
  expect_lt(compare_first_names("KATHLEEN", "KATHERINE"), 0.85)  # two formal names, linked only through KATE
})

test_that("the lexicon can be turned off to reproduce the nickname_table() scores", {
  local({ old <- options(synthid.first_name_lexicon = FALSE); on.exit(options(old))
    expect_equal(compare_first_names("BOB", "ROBERT"), 0.95)
    expect_lt(compare_first_names("KATHLEEN", "KATE"), 0.95)
    expect_lt(compare_first_names("JEFFREY", "JEFF"), 0.95)
  })
  expect_true(is.na(first_name_pair_type("JOHN", "JOSH")))
  expect_equal(first_name_pair_type(c("BOB", "KATE"), c("ROBERT", "KATHLEEN")), c("nickname", "nickname"))
  expect_true(is.na(first_name_pair_type("KATE", "KATHLEEN", lexicon = FALSE)))
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
