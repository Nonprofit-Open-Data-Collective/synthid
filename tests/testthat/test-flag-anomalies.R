## A small linked panel where every person illustrates one anomaly category.
make_anomaly_panel <- function() {
  rec <- function(emp, ein, yr, name, raw = name, title = "BOARD MEMBER",
                  comp = 0, oid = paste0("OID-", ein, "-", yr)) {
    data.frame(
      EMP_ID = emp, ein = ein, org.name = paste0("ORG ", ein), taxyr = yr,
      name = name, name.raw = raw, title.standard = title,
      tot.comp = comp, OBJECTID = oid, stringsAsFactors = FALSE)
  }
  rbind(
    # C1 clean: contiguous tenure, nothing to flag
    rec("C1", "10", 2015, "ANN JONES"), rec("C1", "10", 2016, "ANN JONES"),
    rec("C1", "10", 2017, "ANN JONES"), rec("C1", "10", 2018, "ANN JONES"),
    # H1/H2 co-present homonyms: two JOHN SMITH in org 11 every year
    rec("H1", "11", 2015, "JOHN SMITH", title = "CEO"),
    rec("H2", "11", 2015, "JOHN SMITH", title = "TREASURER"),
    rec("H1", "11", 2016, "JOHN SMITH", title = "CEO"),
    rec("H2", "11", 2016, "JOHN SMITH", title = "TREASURER"),
    # G1 sequential homonym: 4 yrs, 5-yr absence, 4 yrs, pay level jumps 10x
    rec("G1", "12", 2010, "PAT REED", comp = 40000),
    rec("G1", "12", 2011, "PAT REED", comp = 41000),
    rec("G1", "12", 2012, "PAT REED", comp = 42000),
    rec("G1", "12", 2013, "PAT REED", comp = 43000),
    rec("G1", "12", 2019, "PAT REED", comp = 400000),
    rec("G1", "12", 2020, "PAT REED", comp = 410000),
    rec("G1", "12", 2021, "PAT REED", comp = 420000),
    rec("G1", "12", 2022, "PAT REED", comp = 430000),
    # S1 sporadic: 2010, 2013, 2018 only -- span 9, density 0.33
    rec("S1", "13", 2010, "LEE VANCE"), rec("S1", "13", 2013, "LEE VANCE"),
    rec("S1", "13", 2018, "LEE VANCE"),
    # O1 institution modelled as a person: raw person-name blank
    rec("O1", "14", 2015, "REGIONS BANK", raw = ""),
    rec("O1", "14", 2016, "REGIONS BANK", raw = ""),
    # O2 person misfiled into the org-name field: blank raw, but not corporate
    rec("O2", "15", 2015, "SUSAN MCCOY", raw = ""),
    # P1 placeholder name
    rec("P1", "16", 2015, "VACANT"),
    # T1 single-token name
    rec("T1", "17", 2015, "MADONNA"),
    # D1 duplicated Part VII line inside one filing
    rec("D1", "18", 2015, "RAY FOX", title = "DIRECTOR"),
    rec("D2", "18", 2015, "RAY FOX", title = "DIRECTOR")
  )
}

test_that("flag_anomalies finds each category", {
  an <- flag_anomalies(make_anomaly_panel())
  cat_of <- function(id) an$category[match(id, an$emp_id)]
  flag_of <- function(id, f) an[[f]][match(id, an$emp_id)]

  expect_true(is.na(cat_of("C1")))
  expect_false(flag_of("C1", "review_flag"))

  expect_equal(cat_of("H1"), "homonym_copresent")
  expect_equal(cat_of("H2"), "homonym_copresent")
  expect_true(flag_of("H1", "homonym_copresent"))
  ## the clean person in another org must not be swept up
  expect_false(flag_of("C1", "homonym_copresent"))

  expect_equal(cat_of("G1"), "sequential_homonym_pay")
  expect_true(flag_of("G1", "gap_return"))
  expect_true(flag_of("G1", "gap_long"))
  expect_true(flag_of("G1", "gap_pay_shift"))
  expect_equal(flag_of("G1", "max_gap"), 5L)

  expect_true(flag_of("S1", "sporadic"))
  expect_equal(cat_of("O1"), "entity_not_person")
  expect_true(flag_of("O1", "org_as_person"))
  expect_equal(cat_of("P1"), "entity_not_person")
  expect_true(flag_of("P1", "placeholder_name"))
  expect_equal(cat_of("T1"), "name_shape")
  expect_true(flag_of("D1", "dup_line_in_filing"))
})

test_that("name_from_org_field is exact and separate from org_as_person", {
  an <- flag_anomalies(make_anomaly_panel())
  flag_of <- function(id, f) an[[f]][match(id, an$emp_id)]
  ## both came from the org-name field ...
  expect_true(flag_of("O1", "name_from_org_field"))
  expect_true(flag_of("O2", "name_from_org_field"))
  ## ... but only the one carrying a corporate token is an institution
  expect_true(flag_of("O1", "org_as_person"))
  expect_false(flag_of("O2", "org_as_person"))
  ## a person whose name came from the person-name field is untouched
  expect_false(flag_of("C1", "name_from_org_field"))
  expect_equal(flag_of("O2", "category"), "org_field_name")
})

test_that("invariant violations are surfaced, not assumed away", {
  p <- make_anomaly_panel()
  clean <- flag_anomalies(p)
  expect_false(any(clean$dup_id_in_year))
  expect_false(any(clean$id_spans_orgs))

  ## same id twice in one org-year
  bad <- rbind(p, p[p$EMP_ID == "C1" & p$taxyr == 2015, ])
  an <- flag_anomalies(bad)
  expect_true(an$dup_id_in_year[match("C1", an$emp_id)])
  expect_equal(an$category[match("C1", an$emp_id)], "invariant_violation")

  ## one id spanning two organizations
  cross <- p
  cross$EMP_ID[cross$ein == "17"] <- "C1"
  an2 <- flag_anomalies(cross)
  expect_true(an2$id_spans_orgs[match("C1", an2$emp_id)])
})

test_that("optional columns degrade gracefully", {
  p <- make_anomaly_panel()
  bare <- p[, c("EMP_ID", "ein", "taxyr", "name")]
  an <- flag_anomalies(bare)
  expect_s3_class(an, "synthid_anomaly_flags")
  ## no compensation -> the pay flag is never TRUE, but the gap flags still work
  expect_true(all(is.na(an$pay_shift) | !an$gap_pay_shift))
  expect_true(an$gap_return[match("G1", an$emp_id)])
  ## no raw person-name -> the exact org-field check is unavailable
  expect_true(all(is.na(an$name_from_org_field)))
  ## ... but the corporate-token heuristic still catches the institution
  expect_true(an$org_as_person[match("O1", an$emp_id)])
  ## no OBJECTID -> no duplicate-line claims
  expect_false(any(an$dup_line_in_filing))
})

test_that("the review queue holds merge risk, not data-quality items", {
  an <- flag_anomalies(make_anomaly_panel())
  q <- anomaly_review_queue(an)
  expect_true(all(c("H1", "H2", "G1", "S1") %in% q$emp_id))
  expect_true("O1" %in% q$emp_id)                       # entity, included by default
  expect_false("O1" %in% anomaly_review_queue(an, include_entity = FALSE)$emp_id)
  expect_false("D1" %in% q$emp_id)                      # duplicate line is not merge risk
  expect_false("C1" %in% q$emp_id)
  ## ordered by category, then worst-first inside each category
  for (g in split(q$merge_risk, q$category)) expect_false(is.unsorted(rev(g)))
  expect_error(anomaly_review_queue(data.frame()), "flag_anomalies")
})

test_that("thresholds are honoured", {
  p <- make_anomaly_panel()
  strict <- flag_anomalies(p, gap_return_min = 6L, pay_shift_min = 50)
  flag_of <- function(an, id, f) an[[f]][match(id, an$emp_id)]
  expect_false(flag_of(strict, "G1", "gap_return"))     # 5-year gap < 6
  expect_false(flag_of(strict, "G1", "gap_pay_shift"))  # 9x shift < 50
  loose <- flag_anomalies(p, sporadic_density_max = 0.2)
  expect_false(flag_of(loose, "S1", "sporadic"))        # density 0.33 > 0.2
})

test_that("flag_anomalies requires a linked panel", {
  expect_error(flag_anomalies(data.frame(ein = "1", taxyr = 2019, name = "A B")),
               "EMP_ID")
})

test_that("subsetting a flag table still prints", {
  an <- flag_anomalies(make_anomaly_panel())
  expect_output(print(an), "synthid anomaly scan")
  ## Selecting columns drops the summary attribute; the method must fall back to
  ## an ordinary frame rather than report a summary of zeros.
  out <- utils::capture.output(print(an[, c("emp_id", "category")]))
  expect_false(any(grepl("synthid anomaly scan", out)))
  expect_true(any(grepl("emp_id", out)))
  expect_s3_class(anomaly_review_queue(an), "data.frame", exact = TRUE)
})
