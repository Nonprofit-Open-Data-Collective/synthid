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

test_that("compare_last_names() equals compare_two_names() pair by pair", {
  v <- c(NA, "", " ", "-", " - ", "--", "X-", ".", "A", "SMITH", "  SMITH ", "SMYTHE",
         "O'BRIEN", "OBRIEN", "MC LANE", "MCLANE", "MC-LANE", "ANDREWS-MCLANE",
         "ANDREWS MCLANE", "SMITH JR.", "VAN DER BERG", "VANDERBERG", "DE LA CRUZ",
         "ST. JOHN", "ST JOHN", "WEINER-COHEN", "COHEN-GANTSOUDES", "COHEN", "MOORE-SMITH")
  g <- expand.grid(a = v, b = v, stringsAsFactors = FALSE)
  ref <- vapply(seq_len(nrow(g)), function(i) compare_two_names(g$a[i], g$b[i]), numeric(1))
  expect_identical(compare_last_names(g$a, g$b), ref)
  expect_identical(compare_last_names(character(0), character(0)), numeric(0))
})

test_that("surname_blocking_keys() keys a compound surname by whole string and by component", {
  k <- surname_blocking_keys("GARCIA-LOPEZ")
  expect_setequal(k, c("G624", "G620", "L120"))          # GARCIALOPEZ, GARCIA, LOPEZ
  expect_true("G620" %in% intersect(k, surname_blocking_keys("GARCIA")))
  expect_true("L120" %in% intersect(k, surname_blocking_keys("LOPEZ")))
  # splitting on a space works the same way as on a hyphen
  expect_identical(surname_blocking_keys("GARCIA LOPEZ"), k)
})

test_that("surname_blocking_keys() adds nothing for a single-token surname", {
  expect_identical(surname_blocking_keys("SMITH"), "S530")
  expect_identical(surname_blocking_keys("SMYTHE"), "S530")      # Soundex, not exact
  expect_identical(surname_blocking_keys("O'BRIEN"), surname_blocking_keys("OBRIEN"))
})

test_that("surname_blocking_keys() leaves out particles but keeps the whole-string key", {
  k <- surname_blocking_keys("VAN DER BERG")
  expect_setequal(k, c("V536", "B620"))                   # VANDERBERG + BERG; not VAN, DER
  expect_true("V536" %in% surname_blocking_keys("VANDERBERG"))
  expect_true("B620" %in% surname_blocking_keys("BERG"))
  expect_setequal(surname_blocking_keys("MC-LANE"), c("M245", "L500"))  # not MC
  # a particle-only surname still yields its whole-string key
  expect_true(length(surname_blocking_keys("DE LA")) >= 1L)
})

test_that("surname_blocking_keys() takes a variant set and handles empty input", {
  expect_setequal(surname_blocking_keys(c("COHEN", "WEINER")), c("C500", "W560"))
  expect_identical(surname_blocking_keys(character(0)), character(0))
  expect_identical(surname_blocking_keys(c(NA, "")), character(0))
  expect_identical(surname_blocking_keys("-"), character(0))
})

test_that("surname_blocking_keys() does not collide unrelated surnames", {
  expect_length(intersect(surname_blocking_keys("WEINER-COHEN"),
                          surname_blocking_keys("GARCIA-LOPEZ")), 0L)
})
