test_that("preprocess_name splits compound and hyphenated names", {
  expect_equal(preprocess_name("Andrews-McLane"), c("ANDREWS", "MCLANE"))
  expect_equal(preprocess_name("  van der Berg "), c("VAN", "DER", "BERG"))
  expect_equal(preprocess_name(NA_character_), character(0))
  expect_equal(preprocess_name(""), character(0))
})

test_that("compare_two_names is order- and compound-robust", {
  expect_equal(compare_two_names("MCLANE", "MCLANE"), 1)
  # compound vs bare surname aligns on the shared token
  expect_gt(compare_two_names("ANDREWS-MCLANE", "MCLANE"), 0.95)
  # order independence
  expect_equal(
    compare_two_names("ANN MARIE", "MARIE ANN"),
    compare_two_names("MARIE ANN", "ANN MARIE")
  )
  expect_true(is.na(compare_two_names(NA_character_, "SMITH")))
})

test_that("compare_last_names is vectorised", {
  s <- compare_last_names(c("MCLANE", "SMITH"), c("MC-LANE", "JONES"))
  expect_length(s, 2)
  expect_gt(s[1], 0.85)
  expect_lt(s[2], 0.7)
})

test_that("shared single token does not fake a full surname match", {
  # different multi-token surnames that only share one token must score low
  expect_lt(compare_two_names("WEINER-COHEN", "COHEN-GANTSOUDES"), 0.7)
  expect_lt(compare_two_names("HOGARTY-MOTHER", "ANDRADES-MOTHER"), 0.75)
})

test_that("legitimate parse variants still score high", {
  expect_gt(compare_two_names("MCLANE", "MC-LANE"), 0.95)        # separator noise
  expect_gt(compare_two_names("VAN DER BERG", "VANDERBERG"), 0.95)
  expect_gt(compare_two_names("DELACROIX", "DE-LA-CROIX"), 0.95)
  expect_gt(compare_two_names("ANDREWS-MCLANE", "MCLANE"), 0.95) # dropped token
  expect_gt(compare_two_names("COHEN", "COHEN-GANTSOUDES"), 0.95) # added token
  expect_gt(compare_two_names("SMITH-JONES", "JONES-SMITH"), 0.95) # reorder
})
