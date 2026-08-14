make_panel <- function() {
  rec <- function(ein, yr, first, last, suffix, gender, title) {
    nm <- trimws(paste(first, last, suffix))
    data.frame(
      ein = ein, taxyr = yr, name = nm,
      salutation = NA_character_, first_name = first, middle_name = NA_character_,
      last_name = last, suffix = suffix, gender = gender, title.standard = title,
      stringsAsFactors = FALSE
    )
  }
  rbind(
    # org 100
    rec("100", 2019, "JOHN", "SMITH", "SR", "M", "CEO"),
    rec("100", 2019, "JOHN", "SMITH", "JR", "M", "TREASURER"),
    rec("100", 2019, "ANN",  "JONES", "",   "F", "SECRETARY"),
    rec("100", 2020, "JOHN", "SMITH", "SR", "M", "CEO"),
    rec("100", 2020, "JOHN", "SMITH", "JR", "M", "TREASURER"),
    rec("100", 2020, "ANN",  "JONES", "",   "F", "SECRETARY"),
    rec("100", 2021, "JOHN", "SMITH", "SR", "M", "CEO"),
    rec("100", 2021, "JOHN", "SMITH", "JR", "M", "TREASURER"),
    # org 200 -- different org, same-ish name must not cross-link
    rec("200", 2019, "JOHN", "SMITH", "SR", "M", "PRESIDENT"),
    rec("200", 2020, "JOHN", "SMITH", "SR", "M", "PRESIDENT")
  )
}
