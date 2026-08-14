## A small linked panel where every person illustrates one category.
make_flag_panel <- function() {
  rec <- function(emp, ein, org, yr, raw, first, last,
                  mid = "", suf = "", sal = "", oid = NA, tid = NA) {
    data.frame(
      EMP_ID = emp, ein = ein, org.name = org, taxyr = yr,
      name.raw = raw, salutation = sal, first_name = first, middle_name = mid,
      last_name = last, suffix = suf, OBJECTID = oid, TABLE_ID = tid,
      stringsAsFactors = FALSE)
  }
  rbind(
    # P1 nickname / spelling: same surname, Bob<->Robert
    rec("P1", "10", "A", 2019, "Bob Hoey",    "BOB",    "HOEY"),
    rec("P1", "10", "A", 2020, "Robert Hoey", "ROBERT", "HOEY"),
    # P2 order swap: parsed first/last consistent, raw order flips
    rec("P2", "11", "B", 2019, "BRAD SMITH", "BRAD", "SMITH"),
    rec("P2", "11", "B", 2020, "SMITH BRAD", "BRAD", "SMITH"),
    # P3 honorific glued onto given name
    rec("P3", "12", "C", 2019, "William Woolfolk",   "WILLIAM",   "WOOLFOLK"),
    rec("P3", "12", "C", 2020, "MRWILLIAM Woolfolk", "MRWILLIAM", "WOOLFOLK"),
    # P4 credential leaked into surname
    rec("P4", "13", "D", 2019, "Dawn McNeil",             "DAWN", "MCNEIL"),
    rec("P4", "13", "D", 2020, "Dawn McNeil RTR RDMS CRA", "DAWN", "RTR RDMS CRA MCNEIL"),
    # P5 compound / maiden surname
    rec("P5", "14", "E", 2019, "Paul Kipps",         "PAUL", "KIPPS"),
    rec("P5", "14", "E", 2020, "Paul Kennedy Kipps", "PAUL", "KENNEDY-KIPPS"),
    # P6 genuine review: near-miss surname, different people?
    rec("P6", "15", "F", 2019, "Nick Stevens",      "NICK",     "STEVENS"),
    rec("P6", "15", "F", 2020, "Nicholas Stephens", "NICHOLAS", "STEPHENS"),
    # P7 single stray letter glued onto surname (consensus reveals MARTIN)
    rec("P7", "16", "G", 2019, "Robert Martin",   "ROBERT", "MARTIN"),
    rec("P7", "16", "G", 2020, "Robert CMartin",  "ROBERT", "CMARTIN"),
    # P8 singleton -- must be excluded entirely
    rec("P8", "17", "H", 2019, "Jane Roe", "JANE", "ROE")
  )
}

cat_of <- function(flags, id) flags$category[flags$emp_id == id]

test_that("flag_links excludes singletons and evaluates every multi-record person", {
  f <- flag_links(make_flag_panel())
  expect_s3_class(f, "synthid_link_flags")
  expect_false("P8" %in% f$emp_id)          # singleton dropped
  expect_setequal(f$emp_id, paste0("P", 1:7))
})

test_that("flag_links assigns the expected primary category", {
  f <- flag_links(make_flag_panel())
  expect_equal(cat_of(f, "P1"), "nickname_or_spelling")
  expect_equal(cat_of(f, "P2"), "order_swap")
  expect_equal(cat_of(f, "P3"), "parser_honorific_glue")
  expect_equal(cat_of(f, "P4"), "surname_credential")
  expect_equal(cat_of(f, "P5"), "surname_compound")
  expect_equal(cat_of(f, "P6"), "review")
  expect_equal(cat_of(f, "P7"), "parser_honorific_glue")  # single-letter glue
})

test_that("a name that merely starts with a honorific is not called glue", {
  # FRANK / FRED start with 'FR' but are not honorific glue; give them a real
  # reason to be flagged (a surname near-miss) and confirm they route to review.
  df <- data.frame(
    EMP_ID = c("Q", "Q"), ein = c("1", "1"), org.name = c("Z", "Z"),
    taxyr = c(2019, 2020), name.raw = c("Frank Maddox", "Frank Madero"),
    salutation = "", first_name = c("FRANK", "FRANK"), middle_name = "",
    last_name = c("MADDOX", "MADERO"), suffix = "",
    stringsAsFactors = FALSE)
  f <- flag_links(df)
  expect_false(cat_of(f, "Q") == "parser_honorific_glue")
})

test_that("link_review_queue returns only the residual review cases", {
  q <- link_review_queue(flag_links(make_flag_panel()))
  expect_equal(q$emp_id, "P6")
  expect_true(all(q$category == "review"))
  expect_s3_class(q, "data.frame")
  expect_false(inherits(q, "synthid_link_flags"))
})

test_that("parse_fail_log emits input->wrong->expected triples", {
  log <- parse_fail_log(make_flag_panel())

  # honorific glue: MRWILLIAM -> WILLIAM
  h <- log[grepl("honorific_glue", log$defect_type) & log$last_name == "WOOLFOLK", ]
  expect_equal(nrow(h), 1L)
  expect_equal(h$first_name, "MRWILLIAM")
  expect_equal(h$expected_first, "WILLIAM")
  expect_equal(h$source, "self-evident")

  # credential in surname: expected surname is the non-credential remainder
  cr <- log[grepl("credential_in_surname", log$defect_type), ]
  expect_true(grepl("MCNEIL", cr$expected_last))
  expect_true(grepl("RTR", cr$evidence))

  # single-letter glue is consensus-derived: CMARTIN -> MARTIN
  s <- log[grepl("surname_letter_glue", log$defect_type), ]
  expect_equal(s$expected_last, "MARTIN")
  expect_equal(s$source, "consensus")

  # a clean singleton contributes no defects
  expect_false("ROE" %in% log$last_name)
})

test_that("parse_fail_log does not split a name that is itself a real name", {
  # FREDDIE = FR + EDDIE (EDDIE is a known name) must NOT be called honorific glue.
  df <- data.frame(
    EMP_ID = "Z", ein = "1", org.name = "Z", taxyr = 2019,
    name.raw = "Freddie Williams", first_name = "FREDDIE", last_name = "WILLIAMS",
    stringsAsFactors = FALSE)
  log <- parse_fail_log(df)
  expect_false("FREDDIE" %in% log$first_name)
})

test_that("parse_fail_log writes a file when given a path", {
  p <- tempfile(fileext = ".csv")
  out <- parse_fail_log(make_flag_panel(), path = p)
  expect_true(file.exists(p))
  back <- utils::read.csv(p, colClasses = "character")
  expect_equal(nrow(back), nrow(out))
  unlink(p)
})

test_that("parse_fail_tokens rolls the log up into candidate tokens", {
  toks <- parse_fail_tokens(parse_fail_log(make_flag_panel()),
                            known = c("MR", "MRS", "DR"))
  # credentials leaked into the surname are surfaced as novel tokens
  rtr <- toks[toks$token == "RTR", ]
  expect_equal(nrow(rtr), 1L)
  expect_equal(rtr$field, "surname")
  expect_false(rtr$in_known)
  # the honorific that failed to split is recovered and marked known
  mr <- toks[toks$token == "MR" & toks$field == "given", ]
  expect_equal(nrow(mr), 1L)
  expect_true(mr$in_known)
  # a stray-initial glue contributes no token
  expect_false(any(nchar(toks$token) == 1L))
})

test_that("parse_fail_tokens returns an empty frame for a clean log", {
  clean <- data.frame(
    EMP_ID = c("A", "A"), ein = "1", org.name = "X", taxyr = c(2019, 2020),
    name.raw = c("Jane Roe", "Jane Roe"), first_name = "JANE", last_name = "ROE",
    stringsAsFactors = FALSE)
  toks <- parse_fail_tokens(parse_fail_log(clean))
  expect_equal(nrow(toks), 0L)
  expect_true(all(c("token", "in_known") %in% names(toks)))
})

test_that("flag_links handles a panel with no multi-record persons", {
  df <- data.frame(
    EMP_ID = c("A", "B"), ein = c("1", "2"), org.name = c("X", "Y"),
    taxyr = c(2019, 2019), name.raw = c("Jo Ng", "Al Poe"),
    first_name = c("JO", "AL"), last_name = c("NG", "POE"),
    stringsAsFactors = FALSE)
  f <- flag_links(df)
  expect_s3_class(f, "synthid_link_flags")
  expect_equal(nrow(f), 0L)
})
