test_that("build_id_string is unique even for duplicate records", {
  df <- data.frame(
    ein = c("11", "11", "11"),
    taxyr = c(2019, 2019, 2020),
    name = c("Jane Roe", "Jane Roe", "Jane Roe"),
    title.standard = c("CEO", "CEO", "CEO"),
    stringsAsFactors = FALSE
  )
  ids <- build_id_string(df)
  expect_length(ids, 3)
  expect_false(anyDuplicated(ids) > 0)
  expect_true(grepl("-D2$", ids[2]))
})

test_that("build_id_string errors on missing required column", {
  expect_error(build_id_string(data.frame(x = 1)), "not found")
})

test_that("person_year_id is deterministic, unique per source key, and prefixed", {
  oid <- c("OID-1", "OID-1", "OID-2")
  tid <- c("TID-00001", "TID-00002", "TID-00001")
  a <- person_year_id(oid, tid)
  expect_length(a, 3)
  expect_true(all(grepl("^PYID-", a)))
  expect_equal(length(unique(a)), 3L)            # distinct (OBJECTID,TABLE_ID) -> distinct ids
  expect_identical(a, person_year_id(oid, tid))  # stable across calls
  # keyspec is baked in: a recipe bump changes every id
  expect_false(any(person_year_id(oid, tid, keyspec = "v2") == a))
})

test_that("person_year_id errors on mismatched input lengths", {
  expect_error(person_year_id("a", c("x", "y")), "same length")
})

test_that("create_emp_ids is deterministic and content-addressed", {
  a <- create_emp_ids(c("x", "y", "x"))
  expect_equal(a[1], a[3])
  expect_false(a[1] == a[2])
  expect_true(all(grepl("^EMP-", a)))
  # stable across calls
  expect_equal(create_emp_ids("hello"), create_emp_ids("hello"))
})
