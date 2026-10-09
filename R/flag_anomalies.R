#' Token sets used by the anomaly scan
#'
#' `corporate_tokens()` are the words whose presence in a listee name marks it as
#' an institution rather than a person; `placeholder_names()` are the strings
#' filers use in place of a name. Both are plain character vectors so a caller can
#' extend them for a particular corpus.
#'
#' @return A character vector of upper-case tokens.
#' @name synthid_anomaly_tokens
NULL

#' @rdname synthid_anomaly_tokens
#' @export
corporate_tokens <- function() {
  c("INC", "LLC", "LLP", "LP", "CORP", "CORPORATION", "COMPANY", "CO",
    "BANK", "BANCORP", "TRUST", "GROUP", "ASSOC", "ASSOCIATES", "ASSOCIATION",
    "FOUNDATION", "UNIVERSITY", "COLLEGE", "INSTITUTE", "LTD", "PLLC", "PC",
    "SERVICES", "SYSTEM", "SYSTEMS", "CENTER", "CENTRE", "SOCIETY", "PARTNERS",
    "PARTNERSHIP", "CAPITAL", "REALTY", "PROPERTIES", "CHURCH", "SCHOOL",
    "HOSPITAL", "CLINIC", "FUND", "MANAGEMENT", "CONSULTING", "ADVISORS",
    "INSURANCE", "ENTERPRISES", "INDUSTRIES", "HOLDINGS", "CPA", "CPAS",
    "AGENCY", "BUREAU", "COUNCIL", "COMMISSION", "DEPARTMENT", "DISTRICT")
}

#' @rdname synthid_anomaly_tokens
#' @export
placeholder_names <- function() {
  c("BLANK", "NONE", "N/A", "NA", "NONE.", "SAME", "VACANT", "TBD", "UNKNOWN",
    "SEE ATTACHED", "SEE SCHEDULE O", "SEE SCH O", "SEE STATEMENT", "VARIOUS",
    "X", "XX", "XXX", "-", "--", "---", "*", ".")
}

## ---------------------------------------------------------------------------

#' Scan a linked panel for false merges and entity defects
#'
#' Complements [flag_links()], which flags a person cluster only when its member
#' *names disagree*. That makes it a false-**split** detector: a cluster that
#' wrongly fuses two different people who share a name has zero name divergence
#' and is never flagged. This function targets the opposite error, plus the
#' records that should not be person nodes at all.
#'
#' @section Why false merges matter more than they look:
#' Two distinct people sharing a name inside one organization fail in two very
#' different ways.
#' \describe{
#'   \item{co-present}{Both are listed in the *same* years. The linker mints the
#'     correct *number* of ids and threads them arbitrarily between the two
#'     people: individual trajectories are corrupted, but component structure,
#'     node count and degree distribution survive. Flagged as
#'     `homonym_copresent`.}
#'   \item{sequential}{One holds the seat, leaves, and a different person of the
#'     same name appears years later -- one record per year throughout, so greedy
#'     one-to-one links them into a *single* id. This genuinely merges two network
#'     components, which shortens average path length and inflates betweenness
#'     while barely moving density, so it is close to invisible in aggregate
#'     network statistics. Flagged as `gap_return` / `gap_long` /
#'     `gap_pay_shift`.}
#' }
#'
#' @section Flags:
#' \describe{
#'   \item{`dup_id_in_year`, `id_spans_orgs`}{Invariant violations: the same
#'     `EMP_ID` twice in one organization-year, or spanning more than one
#'     organization. [resolve_clusters()] guarantees both are impossible, so a
#'     hit means a bug upstream, not a data anomaly.}
#'   \item{`homonym_copresent`}{The person's name appears on two or more records
#'     in a single organization-year -- proof that at least two distinct people
#'     carry it inside this organization.}
#'   \item{`gap_return`}{An absence of at least `gap_return_min` years inside the
#'     person's span, with at least two observed years on each side.}
#'   \item{`gap_long`}{An absence of at least `gap_long_min` years.}
#'   \item{`gap_pay_shift`}{A gap of two or more years across which mean
#'     compensation shifts by more than `pay_shift_min` (as a proportion of the
#'     earlier mean). Requires `comp`; `NA` when absent or when the person is
#'     never paid. Note that a pay *jump* on its own is a poor signal -- a
#'     volunteer board member becoming paid staff produces one routinely -- which
#'     is why it is only evaluated across a gap.}
#'   \item{`sporadic`}{Span of at least `sporadic_span_min` years but present in
#'     under `sporadic_density_max` of them.}
#'   \item{`name_from_org_field`}{The person's name came from the organization-name
#'     field rather than the person-name field (`raw_name` blank while `name` is
#'     populated). An expansive sanity check on the upstream coalesce, not a
#'     judgement about the name.}
#'   \item{`org_as_person`}{`name_from_org_field` *and* the name carries a
#'     [corporate_tokens()] word: very likely a genuine institution being modelled
#'     as a person, which becomes a fake long-tenure high-degree node.}
#'   \item{`placeholder_name`, `single_token_name`}{The name is a
#'     [placeholder_names()] string, or a lone token.}
#'   \item{`dup_line_in_filing`}{Two records share organization, year, name and
#'     title *within one filing* (same `object_id`). Overwhelmingly a duplicated
#'     Part VII line; kept as distinct records on purpose, since dropping one
#'     would break the 1:1 `PERSON_YEAR_ID` back-link.}
#' }
#'
#' @param df A linked panel: the output of [link_panel()].
#' @param emp_id,org_id,org_name,year Column names for the person id,
#'   organization id, organization name, and tax year.
#' @param name,raw_name Column names for the name actually matched on and for the
#'   literal person-name field from the source filing. `raw_name` is what makes
#'   `name_from_org_field` exact rather than heuristic; when it is absent that
#'   flag and `org_as_person` fall back to name shape alone.
#' @param title,comp,object_id Column names for the standardized title, total
#'   compensation, and the source filing id. Each is optional; the flags that
#'   need one are `NA` when it is missing.
#' @param gap_return_min,gap_long_min Gap lengths (years absent) for `gap_return`
#'   and `gap_long`.
#' @param pay_shift_min Minimum relative change in mean compensation across a gap
#'   for `gap_pay_shift`.
#' @param sporadic_span_min,sporadic_density_max Span and density bounds for
#'   `sporadic`.
#' @param corporate,placeholder Token sets; see [corporate_tokens()] and
#'   [placeholder_names()].
#' @return A data frame with one row per person, class `"synthid_anomaly_flags"`,
#'   ordered worst-first. Carries the tenure summary (`n_records`, `n_years`,
#'   `first_year`, `last_year`, `span`, `density`, `n_gaps`, `max_gap`,
#'   `n_spells`), the flags above, a `merge_risk` score, a primary `category`, and
#'   `review_flag`. Counts and thresholds are attached as the
#'   `"synthid_anomalies"` attribute.
#' @seealso [flag_links()] for the false-split side, [anomaly_review_queue()] for
#'   the merge-risk queue. Base rates measured on `dev/test_panels_1k` are in
#'   `dev/NOTES-dedup-and-anomaly-scan.md`.
#' @examples
#' \dontrun{
#' linked <- link_panel(panel)
#' an <- flag_anomalies(linked)
#' table(an$category)
#' head(anomaly_review_queue(an))
#' }
#' @export
flag_anomalies <- function(df,
                           emp_id = "EMP_ID", org_id = "ein",
                           org_name = "org.name", year = "taxyr",
                           name = "name", raw_name = "name.raw",
                           title = "title.standard", comp = "tot.comp",
                           object_id = "OBJECTID",
                           gap_return_min = 3L, gap_long_min = 5L,
                           pay_shift_min = 1.0,
                           sporadic_span_min = 6L, sporadic_density_max = 0.4,
                           corporate = corporate_tokens(),
                           placeholder = placeholder_names()) {
  stopifnot(is.data.frame(df))
  for (col in c(emp_id, org_id, year, name)) {
    if (!col %in% names(df)) {
      stop("flag_anomalies(): column '", col, "' not found. ",
           "Run link_panel() first.", call. = FALSE)
    }
  }
  up <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x[x == "" | x == "NA"] <- NA
    x
  }
  opt <- function(nm) if (!is.null(nm) && nm %in% names(df)) df[[nm]] else NULL

  d <- data.frame(
    emp_id = as.character(df[[emp_id]]),
    ein    = as.character(df[[org_id]]),
    year   = suppressWarnings(as.integer(df[[year]])),
    nm     = up(df[[name]]),
    stringsAsFactors = FALSE
  )
  d$org_name <- if (!is.null(opt(org_name))) as.character(df[[org_name]]) else NA_character_
  d$title    <- if (!is.null(opt(title)))    up(df[[title]])              else NA_character_
  d$oid      <- if (!is.null(opt(object_id))) as.character(df[[object_id]]) else NA_character_
  d$comp     <- if (!is.null(opt(comp))) suppressWarnings(as.numeric(df[[comp]])) else NA_real_
  ## raw_name blank while name is populated == the name came from the org field.
  has_raw <- !is.null(opt(raw_name))
  d$from_org <- if (has_raw) is.na(up(df[[raw_name]])) & !is.na(d$nm) else NA

  ## ---- record-level defects, later rolled up to the person ------------------
  toks <- strsplit(ifelse(is.na(d$nm), "", d$nm), "[ ,.-]+")
  toks <- lapply(toks, function(v) v[nzchar(v)])
  d$has_corp  <- vapply(toks, function(v) any(v %in% corporate), logical(1))
  d$is_place  <- !is.na(d$nm) & d$nm %in% toupper(placeholder)
  d$one_token <- lengths(toks) == 1L
  ## Without raw_name there is no exact signal, so fall back to name shape.
  d$org_as_person <- if (has_raw) d$from_org & d$has_corp else d$has_corp

  ## ---- co-present homonyms: a name used twice in one organization-year ------
  oy_nm <- paste(d$ein, d$year, d$nm, sep = "\r")
  n_oy  <- stats::ave(seq_len(nrow(d)), oy_nm, FUN = length)
  homonym_names <- unique(paste(d$ein, d$nm, sep = "\r")[n_oy > 1L & !is.na(d$nm)])
  d$homonym <- paste(d$ein, d$nm, sep = "\r") %in% homonym_names

  ## ---- duplicated Part VII line inside a single filing ---------------------
  if (all(is.na(d$oid))) {
    d$dup_line <- FALSE
  } else {
    ln <- paste(d$oid, d$nm, d$title, sep = "\r")
    d$dup_line <- !is.na(d$oid) & stats::ave(seq_len(nrow(d)), ln, FUN = length) > 1L
  }

  ## ---- invariant: the same id twice in one organization-year ---------------
  idy <- paste(d$emp_id, d$ein, d$year, sep = "\r")
  d$dup_id_in_year <- stats::ave(seq_len(nrow(d)), idy, FUN = length) > 1L

  ## ---- per-person roll-up ---------------------------------------------------
  idx <- split(seq_len(nrow(d)), d$emp_id)
  any_true <- function(v) isTRUE(any(v, na.rm = TRUE))
  rows <- lapply(idx, function(r) {
    g  <- d[r, , drop = FALSE]
    yy <- sort(unique(g$year[!is.na(g$year)]))
    gaps <- if (length(yy) > 1L) diff(yy) - 1L else integer(0)
    span <- if (length(yy)) max(yy) - min(yy) + 1L else NA_integer_
    max_gap <- if (length(gaps)) max(gaps) else 0L

    ## Pay level shift across the *largest* gap, as a share of the earlier mean.
    shift <- NA_real_
    if (max_gap >= 2L && any(!is.na(g$comp) & g$comp > 0)) {
      k <- which.max(gaps)
      cut_year <- yy[k]
      before <- g$comp[!is.na(g$comp) & g$comp > 0 & g$year <= cut_year]
      after  <- g$comp[!is.na(g$comp) & g$comp > 0 & g$year >  cut_year]
      if (length(before) && length(after)) {
        shift <- abs(mean(after) - mean(before)) / max(mean(before), 1)
      }
    }
    dens <- if (!is.na(span) && span > 0L) length(yy) / span else NA_real_

    data.frame(
      emp_id = g$emp_id[1L], ein = g$ein[1L], org_name = g$org_name[1L],
      n_records = nrow(g), n_years = length(yy),
      first_year = if (length(yy)) min(yy) else NA_integer_,
      last_year  = if (length(yy)) max(yy) else NA_integer_,
      span = span, density = round(dens, 3),
      n_gaps = sum(gaps > 0L), max_gap = max_gap,
      n_spells = 1L + sum(gaps > 0L),
      pay_shift = round(shift, 3),
      n_orgs = length(unique(g$ein)),
      names_seen = paste(unique(g$nm[!is.na(g$nm)]), collapse = " | "),
      titles_seen = paste(unique(g$title[!is.na(g$title)]), collapse = " | "),
      ## merge-risk flags
      dup_id_in_year    = any_true(g$dup_id_in_year),
      homonym_copresent = any_true(g$homonym),
      ## entity-defect flags
      name_from_org_field = if (has_raw) any_true(g$from_org) else NA,
      org_as_person       = any_true(g$org_as_person),
      placeholder_name    = any_true(g$is_place),
      single_token_name   = any_true(g$one_token),
      dup_line_in_filing  = any_true(g$dup_line),
      stringsAsFactors = FALSE
    )
  })
  res <- do.call(rbind, rows)
  rownames(res) <- NULL

  ## ---- temporal flags, derived from the roll-up ----------------------------
  res$id_spans_orgs <- res$n_orgs > 1L
  ## gap_return wants real tenure on both sides, not a lone record either end.
  res$gap_return <- res$max_gap >= gap_return_min & res$n_years >= 4L
  res$gap_long   <- res$max_gap >= gap_long_min
  res$gap_pay_shift <- !is.na(res$pay_shift) & res$max_gap >= 2L &
    res$pay_shift > pay_shift_min
  res$sporadic <- !is.na(res$density) & res$span >= sporadic_span_min &
    res$density < sporadic_density_max

  merge_flags  <- c("dup_id_in_year", "id_spans_orgs", "homonym_copresent",
                    "gap_return", "gap_long", "gap_pay_shift", "sporadic")
  entity_flags <- c("org_as_person", "placeholder_name", "single_token_name",
                    "name_from_org_field", "dup_line_in_filing")

  ## Weighted so that "proof of two people" outranks "looks intermittent".
  wt <- c(dup_id_in_year = 5, id_spans_orgs = 5, homonym_copresent = 3,
          gap_pay_shift = 2.5, gap_return = 2, gap_long = 1.5, sporadic = 1)
  res$merge_risk <- as.numeric(
    as.matrix(res[names(wt)]) %*% matrix(wt, ncol = 1))

  ## Primary explanation, in priority order.
  res$category <- NA_character_
  set_cat <- function(res, hit, lab) {
    hit <- hit & is.na(res$category)
    res$category[hit] <- lab
    res
  }
  res <- set_cat(res, res$dup_id_in_year | res$id_spans_orgs, "invariant_violation")
  res <- set_cat(res, res$org_as_person | res$placeholder_name, "entity_not_person")
  ## Before homonym_copresent on purpose: two records sharing organization, year,
  ## name and title *inside one filing* are a duplicated Part VII line, which is
  ## the benign mechanical explanation for what would otherwise read as two
  ## same-named people. Same ordering principle as flag_links(): a mechanical
  ## cause outranks the residual "two people?" reading.
  res <- set_cat(res, res$dup_line_in_filing, "duplicate_line")
  res <- set_cat(res, res$homonym_copresent, "homonym_copresent")
  res <- set_cat(res, res$gap_pay_shift, "sequential_homonym_pay")
  res <- set_cat(res, res$gap_return, "sequential_homonym_gap")
  res <- set_cat(res, res$gap_long | res$sporadic, "intermittent")
  res <- set_cat(res, isTRUE_col(res$name_from_org_field), "org_field_name")
  res <- set_cat(res, res$single_token_name, "name_shape")
  res$review_flag <- !is.na(res$category)

  lvl <- c("invariant_violation", "entity_not_person", "duplicate_line",
           "homonym_copresent", "sequential_homonym_pay", "sequential_homonym_gap",
           "intermittent", "org_field_name", "name_shape")
  ord <- factor(res$category, levels = lvl)
  res <- res[order(is.na(res$category), ord, -res$merge_risk), , drop = FALSE]
  rownames(res) <- NULL

  keep <- c("emp_id", "ein", "org_name", "category", "review_flag", "merge_risk",
            "n_records", "n_years", "first_year", "last_year", "span", "density",
            "n_gaps", "max_gap", "n_spells", "pay_shift",
            merge_flags, entity_flags, "names_seen", "titles_seen")
  res <- res[, intersect(keep, names(res)), drop = FALSE]

  flagged <- res[res$review_flag, , drop = FALSE]
  attr(res, "synthid_anomalies") <- list(
    thresholds = list(gap_return_min = gap_return_min, gap_long_min = gap_long_min,
                      pay_shift_min = pay_shift_min,
                      sporadic_span_min = sporadic_span_min,
                      sporadic_density_max = sporadic_density_max),
    n_persons = nrow(res), n_flagged = nrow(flagged),
    n_records = nrow(df),
    has_comp = !all(is.na(d$comp)), has_raw_name = has_raw,
    categories = table(factor(flagged$category, levels = lvl)),
    flag_counts = vapply(res[c(merge_flags, entity_flags)],
                         function(v) sum(v, na.rm = TRUE), numeric(1))
  )
  class(res) <- c("synthid_anomaly_flags", "data.frame")
  res
}

## NA-safe TRUE test for a flag column that may be all-NA (missing input column).
isTRUE_col <- function(v) !is.na(v) & v

#' @export
print.synthid_anomaly_flags <- function(x, ...) {
  r <- attr(x, "synthid_anomalies")
  ## Subsetting drops the summary attribute; fall back to an ordinary frame
  ## rather than printing a summary of zeros.
  if (is.null(r)) return(print.data.frame(x, ...))
  cat("synthid anomaly scan\n--------------------\n")
  cat(sprintf("records              : %d\n", r$n_records %||% 0L))
  cat(sprintf("persons              : %d\n", r$n_persons %||% 0L))
  cat(sprintf("flagged              : %d (%.2f%%)\n", r$n_flagged %||% 0L,
              100 * (r$n_flagged %||% 0L) / max(r$n_persons %||% 1L, 1L)))
  if (!isTRUE(r$has_comp))     cat("  (no compensation column -- gap_pay_shift skipped)\n")
  if (!isTRUE(r$has_raw_name)) cat("  (no raw person-name column -- org-field check is heuristic)\n")
  inv <- sum(x$dup_id_in_year, x$id_spans_orgs, na.rm = TRUE)
  cat(sprintf("invariant violations : %d%s\n", inv,
              if (inv == 0L) "  (expected)" else "  <- BUG, not a data anomaly"))
  cat("by category:\n")
  for (nm in names(r$categories)) {
    if (r$categories[[nm]] > 0L) cat(sprintf("  %-24s %d\n", nm, r$categories[[nm]]))
  }
  cat("flag counts:\n")
  for (nm in names(r$flag_counts)) {
    if (r$flag_counts[[nm]] > 0) cat(sprintf("  %-24s %d\n", nm, r$flag_counts[[nm]]))
  }
  invisible(x)
}

#' The merge-risk review queue
#'
#' The rows of a [flag_anomalies()] result whose category names a possible
#' over-link -- the errors that fuse two network components -- worst first.
#' Entity defects and duplicate lines are excluded: they are data-quality items
#' for the ingest side, not linkage decisions.
#'
#' @param flags A [flag_anomalies()] result.
#' @param include_entity Also return `entity_not_person` rows (institutions and
#'   placeholders modelled as people).
#' @return A data frame (the merge-risk subset), with the class attribute dropped
#'   so it prints as an ordinary frame -- as [link_review_queue()] does.
#' @examples
#' \dontrun{
#' an <- flag_anomalies(link_panel(panel))
#' q <- anomaly_review_queue(an)
#' }
#' @export
anomaly_review_queue <- function(flags, include_entity = TRUE) {
  if (!inherits(flags, "synthid_anomaly_flags")) {
    stop("anomaly_review_queue(): expected the result of flag_anomalies().",
         call. = FALSE)
  }
  cats <- c("invariant_violation", "homonym_copresent",
            "sequential_homonym_pay", "sequential_homonym_gap", "intermittent")
  if (include_entity) cats <- c(cats, "entity_not_person")
  out <- flags[!is.na(flags$category) & flags$category %in% cats, , drop = FALSE]
  class(out) <- "data.frame"
  rownames(out) <- NULL
  out
}
