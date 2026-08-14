make_comparisons <- function(n_match = 80, n_non = 300, seed = 1) {
  set.seed(seed)
  hi <- function(n) pmin(1, pmax(0, stats::rnorm(n, 0.97, 0.03)))
  lo <- function(n) pmin(1, pmax(0, stats::rnorm(n, 0.05, 0.05)))
  m <- data.frame(first_name = hi(n_match), last_name = hi(n_match),
                  gender = hi(n_match), middle_name = hi(n_match))
  u <- data.frame(first_name = lo(n_non), last_name = lo(n_non),
                  gender = c(hi(n_non %/% 2), lo(n_non - n_non %/% 2)), # gender agrees by chance
                  middle_name = lo(n_non))
  cmp <- rbind(m, u)
  attr(cmp, "features") <- c("first_name", "last_name", "gender", "middle_name")
  list(cmp = cmp, is_match = c(rep(TRUE, n_match), rep(FALSE, n_non)))
}

test_that("EM recovers m > u and a sensible prior", {
  d <- make_comparisons()
  em <- fit_match_model(d$cmp, method = "em")
  expect_s3_class(em, "synthid_model")
  expect_true(all(em$m > em$u))
  expect_gt(em$p, 0); expect_lt(em$p, 1)
  # informative fields get positive agreement weight; gender (agrees by chance) weakest
  w <- fs_weights(em)
  expect_true(all(w$w_agree[w$feature %in% c("first_name", "last_name")] > 1))
  expect_lt(w$w_agree[w$feature == "gender"], w$w_agree[w$feature == "first_name"])
})

test_that("EM posterior separates matches from non-matches and is calibrated", {
  d <- make_comparisons()
  em <- fit_match_model(d$cmp, method = "em")
  p <- predict_match(em, d$cmp)
  expect_true(all(p >= 0 & p <= 1))
  expect_gt(mean(p[d$is_match]), 0.9)
  expect_lt(mean(p[!d$is_match]), 0.1)
})

test_that("missing fields contribute no evidence", {
  d <- make_comparisons()
  em <- fit_match_model(d$cmp, method = "em")
  one <- d$cmp[1, , drop = FALSE]
  full <- predict_match(em, one)
  one$last_name <- NA
  dropped <- predict_match(em, one)
  # removing a strong agreeing field should lower (not raise) the posterior
  expect_lte(dropped, full + 1e-9)
})

test_that("logistic path fits and predicts probabilities", {
  d <- make_comparisons()
  lg <- suppressWarnings(
    fit_match_model(d$cmp, method = "logistic", labels = as.integer(d$is_match))
  )
  p <- predict_match(lg, d$cmp)
  expect_true(all(p >= 0 & p <= 1))
  expect_gt(mean(p[d$is_match]), mean(p[!d$is_match]))
})

test_that("fs_weights only applies to EM models", {
  d <- make_comparisons()
  lg <- suppressWarnings(fit_match_model(d$cmp, method = "logistic",
                                         labels = as.integer(d$is_match)))
  expect_error(fs_weights(lg), "EM")
})

test_that("link_panel(method='em') runs and adds a confidence column", {
  panel <- make_panel()  # from test-link-panel.R helper
  linked <- link_panel(panel, method = "em")
  expect_true(all(c("EMP_ID", "EMP_LINK_CONF") %in% names(linked)))
  key <- paste(linked$EMP_ID, linked$ein, linked$taxyr)
  expect_false(anyDuplicated(key) > 0)                # invariant holds
  conf <- linked$EMP_LINK_CONF[!is.na(linked$EMP_LINK_CONF)]
  expect_true(all(conf >= 0 & conf <= 1))
})
