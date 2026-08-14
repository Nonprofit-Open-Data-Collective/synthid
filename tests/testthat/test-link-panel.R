## make_panel() is defined in helper-make-panel.R (shared across test files).
## It builds a small panel exercising the hard cases: a two-person same-surname
## family board (John Smith Sr / Jr), a person absent in the middle year (Ann
## Jones, 2019 & 2021), and a second org that must never cross-link.

test_that("link_panel produces stable per-person ids", {
  panel <- make_panel()
  linked <- link_panel(panel)

  expect_true(all(c("EMP_ID", "EMP_N_RECORDS", "EMP_N_YEARS") %in% names(linked)))
  expect_equal(nrow(linked), nrow(panel))

  emp <- linked$EMP_ID
  # Sr in org 100 (rows 1,4,7) is one person
  expect_equal(length(unique(emp[c(1, 4, 7)])), 1L)
  # Jr in org 100 (rows 2,5,8) is one person
  expect_equal(length(unique(emp[c(2, 5, 8)])), 1L)
  # Sr and Jr are NOT the same person despite the shared surname
  expect_false(emp[1] == emp[2])
  # Ann appears 2019 & 2021 (rows 3,6) -- linked across the gap year
  expect_equal(length(unique(emp[c(3, 6)])), 1L)
})

test_that("no cluster spans two organizations", {
  panel <- make_panel()
  linked <- link_panel(panel)
  org100 <- linked$EMP_ID[linked$ein == "100"]
  org200 <- linked$EMP_ID[linked$ein == "200"]
  expect_length(intersect(org100, org200), 0L)
})

test_that("one-person-per-org-year invariant holds", {
  panel <- make_panel()
  linked <- link_panel(panel)
  key <- paste(linked$EMP_ID, linked$ein, linked$taxyr)
  expect_false(anyDuplicated(key) > 0)
})

test_that("EMP_ID is deterministic across runs", {
  panel <- make_panel()
  a <- link_panel(panel)$EMP_ID
  b <- link_panel(panel)$EMP_ID
  expect_equal(a, b)
})
