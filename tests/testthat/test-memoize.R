test_that("memoize_comparator returns byte-identical results to the raw comparator", {
  set.seed(1)
  x <- sample(c("SMITH", "MC-LANE", "VAN DER BERG", "COHEN-GANTSOUDES", NA), 60, TRUE)
  y <- sample(c("SMYTHE", "MCLANE", "VANDERBERG", "COHEN", NA), 60, TRUE)
  raw <- compare_last_names(x, y)
  m   <- memoize_comparator(compare_last_names)
  expect_identical(m(x, y), raw)   # first pass: all cache misses
  expect_identical(m(x, y), raw)   # second pass: all cache hits

  fx <- sample(c("BOB", "ROBERT", "JON", "J", "ALEXIS", NA), 60, TRUE)
  fy <- sample(c("ROBERT", "BOB", "JONATHAN", "JOHN", "MARCELLA", NA), 60, TRUE)
  rawf <- compare_first_names(fx, fy)
  mf   <- memoize_comparator(compare_first_names)
  expect_identical(mf(fx, fy), rawf)
  expect_identical(mf(fx, fy), rawf)
})

test_that("memoized linkage yields identical EMP_IDs to the unmemoized path", {
  panel <- make_panel()
  base <- withr::with_options(
    list(synthid.memoize_comparators = FALSE), link_panel(panel))
  memo <- withr::with_options(
    list(synthid.memoize_comparators = TRUE), link_panel(panel))
  expect_identical(memo$EMP_ID, base$EMP_ID)
  expect_identical(memo$EMP_N_YEARS, base$EMP_N_YEARS)
})

test_that("memoize wrapper survives being built in a reassigning loop (no self-recursion)", {
  # Guards the force(fn) fix: wrapping in a loop that overwrites the slot must
  # not leave `fn` pointing at the wrapper itself.
  cmps <- list(a = compare_last_names, b = compare_first_names)
  for (f in names(cmps)) cmps[[f]] <- memoize_comparator(cmps[[f]])
  expect_identical(cmps$a("SMITH", "SMITH"), compare_last_names("SMITH", "SMITH"))
  expect_identical(cmps$b("BOB", "ROBERT"), compare_first_names("BOB", "ROBERT"))
})
