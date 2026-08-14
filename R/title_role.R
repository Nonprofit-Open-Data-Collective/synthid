#' Coarse role taxonomy over standardized titles
#'
#' Collapses the ~250-value `title.standard` vocabulary (produced by
#' \pkg{titleclassifier}) into four coarse roles used by cross-organization
#' linkage and by interlock studies:
#' \describe{
#'   \item{`BOARD`}{Governance body and its officers -- board/trustee members and
#'     the elected governance officers (president, vice president, secretary,
#'     treasurer, chair) whether or not prefixed with `BOARD`. Per the interlock
#'     use case, the governance triad (president/treasurer/secretary) is treated
#'     as `BOARD`, **not** `OFFICER`.}
#'   \item{`OFFICER`}{Paid executive leadership -- the C-suite (`CHIEF ... OFFICER`,
#'     `CEO`/`CFO`/`COO`/...), executive director, general manager, general
#'     counsel, comptroller. These are the *management* officers, distinct from the
#'     volunteer governance officers above.}
#'   \item{`STAFF`}{Other paid employees -- functional directors (`DIRECTOR OF ...`,
#'     `MEDICAL DIRECTOR`), functional vice presidents (`VICE PRESIDENT OF ...`),
#'     managers, coordinators, administrators, clerks, generic employees.}
#'   \item{`OTHER`}{Anything not matched above -- role-specific or organization-
#'     specific titles (e.g. `CHAPLAIN`, `SERGEANT AT ARMS`, `HISTORIAN`,
#'     `CONSULTANT`, `MILITARY`). Unmatched titles land here by design so the
#'     classifier is safe on the tail of the vocabulary; audit them with
#'     [title_role_coverage()] and pin exceptions via the `overrides` argument.}
#' }
#'
#' The distinction that does the work: a governance title carrying a *functional*
#' qualifier (`... OF FINANCE`, `... OF MARKETING`) is a paid post, not a
#' governance seat, so any title containing `" OF "` is excluded from `BOARD`
#' (`VICE PRESIDENT` -> `BOARD`, but `VICE PRESIDENT OF FINANCE` -> `STAFF`).
#'
#' @name title_role
NULL

## Governance "base" roles. A whole title is BOARD when it is one of these,
## optionally carrying a leading positional modifier (VICE, ASSISTANT, PAST, an
## ordinal, ...) and/or a `BOARD ` prefix -- and it does not carry a functional
## `OF ...` qualifier.
.governance_bases <- function() {
  c("PRESIDENT", "VICE PRESIDENT", "VP", "SECRETARY", "TREASURER",
    "SECRETARY-TREASURER", "SECRETARY/TREASURER", "SECRETARY TREASURER",
    "CHAIR", "CHAIRMAN", "CHAIRPERSON", "CHAIRWOMAN", "CHAIRMAN OF THE BOARD",
    "DIRECTOR", "TRUSTEE", "MEMBER", "BOARD MEMBER", "BOARD",
    "PRESIDENT-ELECT", "PRESIDENT ELECT", "GOVERNOR", "REGENT", "OVERSEER",
    "ELDER", "DEACON", "COMMISSIONER")
}

## Positional modifiers that may lead a governance base without changing that it
## is a governance seat (a "2nd vice president" is still board governance).
.governance_modifiers <- function() {
  c("VICE", "CO", "FIRST", "SECOND", "THIRD", "FOURTH",
    "1ST", "2ND", "3RD", "4TH", "SR", "JR", "SENIOR", "JUNIOR",
    "ASSISTANT", "ASST", "ASSOCIATE", "ASSOC", "DEPUTY",
    "PAST", "IMMEDIATE PAST", "INCOMING", "OUTGOING",
    "ACTING", "INTERIM", "HONORARY", "EMERITUS", "EX OFFICIO", "BOARD")
}

#' Regex rules behind [classify_title_role()]
#'
#' Exposed so the rule set is inspectable and testable. Each element is an
#' anchored, upper-case regular expression; [classify_title_role()] applies them
#' in the documented precedence (BOARD, then OFFICER, then STAFF).
#'
#' @return A named list of regular expressions.
#' @export
title_role_rules <- function() {
  mods <- paste(.governance_modifiers(), collapse = "|")
  bases <- paste(vapply(.governance_bases(), function(b) gsub("([./-])", "\\\\\\1", b),
                        character(1)), collapse = "|")
  list(
    ## Governance: optional stacked modifiers, then a governance base, whole string.
    board = paste0("^((", mods, ") )*(", bases, ")$"),
    ## Paid executive leadership.
    officer = paste(
      "^C[A-Z]?[EFOAITDMR]O$",              # CEO CFO COO CIO CAO ... (2-3 letter)
      "CHIEF .*OFFICER",                     # CHIEF _X_ OFFICER
      "EXECUTIVE DIRECTOR",
      "EXECUTIVE VICE PRESIDENT",
      "GENERAL MANAGER",
      "MANAGING DIRECTOR",
      "GENERAL COUNSEL",
      "COMPTROLLER", "CONTROLLER",
      "PRESIDENT (AND|&|/) ?(CEO|C\\.E\\.O)",
      sep = "|"),
    ## Other paid staff.
    staff = paste(
      " OF ",                                # functional VP/director: "... OF FINANCE"
      "DIRECTOR$", "DIRECTOR ",              # MEDICAL DIRECTOR, PROGRAM DIRECTOR, DIRECTOR OF ...
      "MANAGER", "COORDINATOR", "ADMINISTRATOR", "SUPERVISOR",
      "CLERK", "EMPLOYEE", "^STAFF$", "ASSISTANT", "WEBMASTER",
      "ANALYST", "SPECIALIST", "OFFICER", "ADVISOR",
      sep = "|")
  )
}

## Normalize a raw title for matching: upper-case, collapse internal whitespace.
.norm_title <- function(x) {
  x <- toupper(trimws(as.character(x)))
  gsub("\\s+", " ", x)
}

#' Assign a coarse role to standardized titles
#'
#' Vectorized classifier mapping each `title.standard` value to one of
#' `BOARD`, `OFFICER`, `STAFF`, `OTHER` (see [title_role]). Precedence is
#' overrides, then `BOARD`, then `OFFICER`, then `STAFF`, then `OTHER`.
#'
#' @param title Character vector of titles (any case; whitespace is squished).
#' @param overrides Optional named character vector mapping an *exact* normalized
#'   title to a role, e.g. `c("SERGEANT AT ARMS" = "BOARD")`. Overrides win over
#'   every rule, so this is the one-line escape hatch for tail cases the rules get
#'   wrong. Names are matched after the same normalization applied to `title`.
#' @param levels If `TRUE` (default) return a factor with levels
#'   `c("BOARD","OFFICER","STAFF","OTHER")`; otherwise a character vector.
#' @return A factor (or character vector) the same length as `title`; `NA` titles
#'   map to `NA`.
#' @examples
#' classify_title_role(c("BOARD PRESIDENT", "BOARD SECRETARY", "TREASURER",
#'                        "CHIEF FINANCIAL OFFICER", "EXECUTIVE DIRECTOR",
#'                        "VICE PRESIDENT", "VICE PRESIDENT OF FINANCE",
#'                        "DIRECTOR OF MARKETING", "CHAPLAIN"))
#' @seealso [title_role_coverage()] to audit a whole vocabulary.
#' @export
classify_title_role <- function(title, overrides = NULL, levels = TRUE) {
  lv <- c("BOARD", "OFFICER", "STAFF", "OTHER")
  t <- .norm_title(title)
  out <- rep(NA_character_, length(t))
  present <- !is.na(t) & nzchar(t)

  rules <- title_role_rules()
  ## Precedence: BOARD, then OFFICER, then STAFF, else OTHER.
  unset <- present & is.na(out)
  out[unset & grepl(rules$board, t)] <- "BOARD"
  unset <- present & is.na(out)
  out[unset & grepl(rules$officer, t)] <- "OFFICER"
  unset <- present & is.na(out)
  out[unset & grepl(rules$staff, t)] <- "STAFF"
  out[present & is.na(out)] <- "OTHER"

  ## Overrides win over everything (applied last so they cannot be masked).
  if (length(overrides)) {
    if (is.null(names(overrides))) {
      stop("classify_title_role(): `overrides` must be a *named* vector ",
           "(title = role).", call. = FALSE)
    }
    bad <- setdiff(unique(toupper(overrides)), lv)
    if (length(bad)) {
      stop("classify_title_role(): override role(s) not in ",
           paste(lv, collapse = "/"), ": ", paste(bad, collapse = ", "),
           call. = FALSE)
    }
    key <- .norm_title(names(overrides))
    hit <- match(t, key)
    ov <- !is.na(hit)
    out[ov] <- toupper(overrides[hit[ov]])
  }

  if (levels) factor(out, levels = lv) else out
}

#' Audit the role assignment for a title vocabulary
#'
#' Tabulates a set of raw titles, classifies each distinct value, and returns the
#' assignment sorted by frequency -- the tool for reviewing the full ~250-value
#' vocabulary and finding the tail that lands in `OTHER` (or is misclassified)
#' before pinning exceptions with the `overrides` argument of
#' [classify_title_role()].
#'
#' @param titles Character vector of raw (possibly repeated) titles.
#' @param overrides Passed through to [classify_title_role()].
#' @return A data frame: `title` (distinct normalized value), `n` (count),
#'   `role`, ordered by descending `n`.
#' @examples
#' \dontrun{
#' cov <- title_role_coverage(panel$title.standard)
#' subset(cov, role == "OTHER")           # the tail to review
#' aggregate(n ~ role, cov, sum)          # role mass
#' }
#' @export
title_role_coverage <- function(titles, overrides = NULL) {
  t <- .norm_title(titles)
  t <- t[!is.na(t) & nzchar(t)]
  tb <- sort(table(t), decreasing = TRUE)
  data.frame(
    title = names(tb),
    n = as.integer(tb),
    role = classify_title_role(names(tb), overrides = overrides, levels = FALSE),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
