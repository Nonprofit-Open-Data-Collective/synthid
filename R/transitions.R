## Position transitions, promotions, and churn -----------------------------------
##
## Three post-linkage analytics that only become answerable once link_panel() has
## stamped an EMP_ID on every record: they read *change over time* off the person
## clusters. All three operate within an organization (EMP_ID is a within-org
## cross-year id) and share one position matcher, so "the CEO seat" or "the board"
## is selected the same way everywhere.

## ---- shared helpers ---------------------------------------------------------

## Normalize a title/position label for matching: upper-case, squish internal
## whitespace, blanks -> NA. (Local copy so this file is self-contained.)
.norm_pos <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- gsub("\\s+", " ", x)
  x[is.na(x) | x == ""] <- NA_character_
  x
}

.check_linked <- function(df, emp_id) {
  stopifnot(is.data.frame(df))
  if (!emp_id %in% names(df)) {
    stop(sprintf("id column '%s' not found. Run link_panel() first.", emp_id),
         call. = FALSE)
  }
}

## The common row selector behind flag_position_transitions() and
## position_churn(). A "position" can be named two ways, combined with OR:
##   * `position` -- one or more standardized-title strings, matched against the
##     `title` column under `match` ("exact" normalized equality, "regex"
##     alternation, or "fuzzy" Jaro-Winkler at `min_sim`).
##   * `role` -- one or more coarse roles (BOARD/OFFICER/STAFF/OTHER) from
##     classify_title_role(); the natural way to select a whole governance body
##     ("the board") that spans many distinct titles.
## Returns a logical vector, nrow(df) long, TRUE on rows that hold the position.
.match_position <- function(df, position = NULL, role = NULL,
                            title = "title.standard",
                            match = c("exact", "regex", "fuzzy"),
                            min_sim = 0.90, overrides = NULL) {
  match <- match.arg(match)
  if (is.null(position) && is.null(role)) {
    stop("Provide `position` (title[s]) and/or `role` (coarse role[s]).",
         call. = FALSE)
  }
  if (!title %in% names(df)) {
    stop(sprintf("title column '%s' not found in data frame.", title), call. = FALSE)
  }
  ttl <- .norm_pos(df[[title]])
  mask <- rep(FALSE, nrow(df))

  if (!is.null(role)) {
    role <- toupper(trimws(role))
    r <- as.character(classify_title_role(df[[title]], overrides = overrides,
                                          levels = FALSE))
    mask <- mask | (!is.na(r) & r %in% role)
  }
  if (!is.null(position)) {
    pos <- .norm_pos(position)
    pos <- pos[!is.na(pos)]
    if (length(pos)) {
      if (match == "exact") {
        mask <- mask | (!is.na(ttl) & ttl %in% pos)
      } else if (match == "regex") {
        mask <- mask | grepl(paste(pos, collapse = "|"), ttl)
      } else {
        sim <- vapply(pos, function(p) stringdist::stringsim(ttl, p, method = "jw"),
                      numeric(length(ttl)))
        if (is.null(dim(sim))) sim <- matrix(sim, ncol = length(pos))
        best <- apply(sim, 1L, function(v) if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE))
        mask <- mask | (!is.na(best) & best >= min_sim)
      }
    }
  }
  mask & !is.na(ttl)
}

## Jaro-Winkler similarity of two scalars, NA-safe.
.jw <- function(a, b) {
  if (is.na(a) || is.na(b)) return(NA_real_)
  stringdist::stringsim(a, b, method = "jw")
}

## ---- 1. position transitions ------------------------------------------------

#' Flag position transitions -- when the person holding a title changes
#'
#' For a position that is normally held by one or two people (a CEO seat, a board
#' presidency), tracks *who* holds it over an organization's filing years and flags
#' the years where a **new** `EMP_ID` takes over from a different holder. This is
#' the succession signal: the title stays put, the person underneath it changes.
#'
#' What counts as "the previous year" is set by `gap_tolerant`:
#' \describe{
#'   \item{`FALSE` (default, strict)}{The organization's filing timeline (every year
#'     the org appears in `df`) defines the previous year. An incoming holder is a
#'     *transition* only when the seat was actually occupied by someone else in the
#'     prior **filing** year; a holder appearing after a vacant seat-year (or the
#'     first time the org reports the position) is an establishment -- `incoming`
#'     but not `transition`. Use this when a vacancy should not be read as a
#'     succession.}
#'   \item{`TRUE` (gap-tolerant)}{The previous year is the most recent year the seat
#'     was actually **occupied**, however many vacant or non-filing years intervene.
#'     A different person refilling a seat that sat empty for years is then a
#'     transition, its predecessor the last person who held it. Use this to follow a
#'     seat across gaps. `years_since_prev` reports the span bridged.}
#' }
#' Either way, the very first occupant of a seat is `incoming` but never a
#' `transition` (no prior holder to succeed).
#'
#' Select the position by `position` (one or more standardized-title strings) and/or
#' `role` (one or more coarse roles from [classify_title_role()]); the two combine
#' with OR. For a one-or-two-holder seat exact title matching is usually what you
#' want; `role = "BOARD"` selects the whole body (better measured with
#' [position_churn()]).
#'
#' @param df A linked panel: output of [link_panel()], one record per
#'   organization-year per person, carrying `EMP_ID`.
#' @param position Character vector of standardized titles (or `NULL`).
#' @param role Character vector of coarse roles -- `"BOARD"`, `"OFFICER"`,
#'   `"STAFF"`, `"OTHER"` (or `NULL`). At least one of `position`/`role` is required.
#' @param match How `position` is matched against `title`: `"exact"` (normalized
#'   equality, the default), `"regex"` (alternation), or `"fuzzy"` (Jaro-Winkler
#'   at `min_sim`).
#' @param min_sim Similarity floor for `match = "fuzzy"`.
#' @param gap_tolerant If `TRUE`, measure succession against the last **occupied**
#'   seat-year rather than the prior filing year, so a seat refilled after a
#'   vacancy counts as a transition (default `FALSE`).
#' @param overrides Passed to [classify_title_role()] when selecting by `role`.
#' @param cols Column mapping; see [synthid_cols()] (uses `org_id`, `year`, `name`).
#' @param emp_id Person-cluster id column (default `"EMP_ID"`).
#' @param title Standardized-title column (default `"title.standard"`).
#' @return A data frame of class `synthid_transitions`, one row per
#'   (organization, filing-year, holder) of the position, ordered by org then year:
#'   \describe{
#'     \item{org id, year (named per `cols`), `EMP_ID`, `name`, `title`}{The holder
#'       and the matched standardized title.}
#'     \item{`prev_year`}{The reference year succession is measured against -- the
#'       prior filing year (strict) or the last occupied seat-year (gap-tolerant);
#'       `NA` for a seat's first occupant.}
#'     \item{`years_since_prev`}{`year - prev_year`; `1` in the strict case unless a
#'       filing gap intervenes, larger when a gap-tolerant match bridges vacant
#'       years.}
#'     \item{`n_holders`, `n_prev_holders`}{Seat occupancy this and the reference
#'       year.}
#'     \item{`incoming`}{Holder did not hold the seat in the reference year.}
#'     \item{`continuing`}{Holder also held it in the reference year.}
#'     \item{`transition`}{`incoming` **and** the seat had a different occupant in
#'       the reference year -- a genuine handover.}
#'     \item{`predecessor`}{On a transition row, the `EMP_ID`(s) present in the
#'       reference year but gone this year (the person(s) replaced), `;`-joined;
#'       `NA` otherwise.}
#'   }
#'   A run summary is attached as attribute `"synthid_transitions"`.
#' @examples
#' \dontrun{
#' linked <- link_panel(panel)
#' tr <- flag_position_transitions(linked, position = "EXECUTIVE DIRECTOR")
#' subset(tr, transition)                       # just the handovers
#' flag_position_transitions(linked, position = c("CEO", "CHIEF EXECUTIVE OFFICER"))
#' }
#' @seealso [position_churn()] for body-level turnover, [flag_promotions()] for
#'   a person changing title.
#' @export
flag_position_transitions <- function(df, position = NULL, role = NULL,
                                      cols = synthid_cols(),
                                      emp_id = "EMP_ID",
                                      title = "title.standard",
                                      match = c("exact", "regex", "fuzzy"),
                                      min_sim = 0.90, gap_tolerant = FALSE,
                                      overrides = NULL) {
  match <- match.arg(match)
  .check_linked(df, emp_id)
  for (nm in c(cols$org_id, cols$year)) {
    if (!nm %in% names(df)) {
      stop(sprintf("column '%s' not found in data frame.", nm), call. = FALSE)
    }
  }

  org  <- as.character(df[[cols$org_id]])
  yr   <- suppressWarnings(as.integer(df[[cols$year]]))
  id   <- as.character(df[[emp_id]])
  name <- if (cols$name %in% names(df)) as.character(df[[cols$name]]) else rep(NA_character_, nrow(df))
  ttl  <- as.character(df[[title]])

  hold <- .match_position(df, position, role, title, match, min_sim, overrides)
  keep <- hold & !is.na(org) & !is.na(yr) & !is.na(id)

  empty <- .empty_transitions(cols)
  if (!any(keep)) return(.as_transitions(empty, n_orgs = 0L))

  ## Organization filing timeline (all years the org appears, not just seat years).
  org_years <- lapply(split(yr, org), function(v) sort(unique(v[!is.na(v)])))

  sub <- data.frame(org = org[keep], yr = yr[keep], id = id[keep],
                    name = name[keep], title = ttl[keep], stringsAsFactors = FALSE)
  ## One holder row per (org, year, id): a person can only hold the seat once a year.
  sub <- sub[!duplicated(sub[c("org", "yr", "id")]), , drop = FALSE]

  oy_key <- function(o, y) paste(o, y, sep = "\r")
  hset   <- lapply(split(sub$id, oy_key(sub$org, sub$yr)), unique)
  ## Years the seat is actually occupied, per org -- the timeline for gap-tolerance.
  seat_years <- lapply(split(sub$yr, sub$org), function(v) sort(unique(v)))

  n <- nrow(sub)
  prev_year   <- integer(n)
  n_holders   <- integer(n)
  n_prev      <- integer(n)
  incoming    <- logical(n)
  transition  <- logical(n)
  predecessor <- rep(NA_character_, n)

  for (i in seq_len(n)) {
    o <- sub$org[i]; y <- sub$yr[i]
    ## Reference year: last occupied seat-year (gap-tolerant) or prior filing year.
    ref <- if (gap_tolerant) seat_years[[o]] else org_years[[o]]
    ref <- ref[ref < y]
    py <- if (length(ref)) max(ref) else NA_integer_
    prev_year[i] <- py
    cur  <- hset[[oy_key(o, y)]]
    prev <- if (!is.na(py)) hset[[oy_key(o, py)]] else NULL
    prev <- if (is.null(prev)) character(0) else prev
    n_holders[i] <- length(cur)
    n_prev[i]    <- length(prev)
    incoming[i]  <- !(sub$id[i] %in% prev)
    transition[i] <- incoming[i] && length(prev) > 0L
    if (transition[i]) {
      gone <- setdiff(prev, cur)
      if (length(gone)) predecessor[i] <- paste(sort(gone), collapse = ";")
    }
  }

  out <- data.frame(
    org = sub$org, yr = sub$yr, EMP_ID = sub$id, name = sub$name, title = sub$title,
    prev_year = prev_year, years_since_prev = sub$yr - prev_year,
    n_holders = n_holders, n_prev_holders = n_prev,
    incoming = incoming, continuing = !incoming, transition = transition,
    predecessor = predecessor, stringsAsFactors = FALSE
  )
  names(out)[1:2] <- c(cols$org_id, cols$year)
  out <- out[order(out[[cols$org_id]], out[[cols$year]], out$EMP_ID), , drop = FALSE]
  rownames(out) <- NULL
  .as_transitions(out, n_orgs = length(unique(out[[cols$org_id]])))
}

.empty_transitions <- function(cols) {
  out <- data.frame(
    a = character(0), b = integer(0), EMP_ID = character(0), name = character(0),
    title = character(0), prev_year = integer(0), years_since_prev = integer(0),
    n_holders = integer(0), n_prev_holders = integer(0), incoming = logical(0),
    continuing = logical(0), transition = logical(0), predecessor = character(0),
    stringsAsFactors = FALSE
  )
  names(out)[1:2] <- c(cols$org_id, cols$year)
  out
}

.as_transitions <- function(out, n_orgs) {
  attr(out, "synthid_transitions") <- list(
    n_orgs        = n_orgs,
    n_holder_rows = nrow(out),
    n_incoming    = sum(out$incoming, na.rm = TRUE),
    n_transitions = sum(out$transition, na.rm = TRUE)
  )
  class(out) <- c("synthid_transitions", "data.frame")
  out
}

#' @export
print.synthid_transitions <- function(x, ...) {
  r <- attr(x, "synthid_transitions")
  if (is.null(r)) { NextMethod(); return(invisible(x)) }  # e.g. after column subsetting
  cat("synthid position transitions\n----------------------------\n")
  cat(sprintf("organizations   : %d\n", r$n_orgs %||% 0L))
  cat(sprintf("holder rows     : %d\n", r$n_holder_rows %||% 0L))
  cat(sprintf("incoming holders: %d\n", r$n_incoming %||% 0L))
  cat(sprintf("transitions     : %d  (new person takes an occupied seat)\n",
              r$n_transitions %||% 0L))
  NextMethod()
}

## ---- 2. promotions ----------------------------------------------------------

## Coarse seniority rank used only to guess promotion direction when compensation
## is uninformative. Governance (BOARD) is off the paid ladder -> NA.
.role_rank <- function(role) {
  r <- c(OFFICER = 3L, STAFF = 2L, OTHER = 1L)
  unname(ifelse(role %in% names(r), r[role], NA_integer_))
}

#' Flag promotions -- when a person's title changes for real
#'
#' Walks each person's (`EMP_ID`) year-ordered title history and flags the changes
#' that are genuine role moves, separating them from the noise that makes a naive
#' "title changed => promotion" rule wrong. Titles are not reported consistently
#' from year to year, so a raw diff over-counts wildly; this function inspects the
#' person's *upstream* history and both the raw and standardized labels to sort each
#' change into one of:
#' \describe{
#'   \item{`relabel`}{The two labels are near-identical under Jaro-Winkler on
#'     **either** the standardized or the raw title (`>= min_sim`) -- a spelling or
#'     formatting variant of the same post, not a move.}
#'   \item{`oscillation`}{The "new" title fuzzy-matches a title the person already
#'     held in an **earlier** year (before the prior year) -- the record is
#'     flip-flopping between inconsistent labels, not advancing.}
#'   \item{`transient`}{A novel title that lasts a single year and then **reverts**
#'     to the prior title next year -- a one-year labeling blip.}
#'   \item{`real`}{A novel title, not a relabel, not seen upstream, that does not
#'     immediately revert. Direction (`promotion` / `demotion` / `lateral`) is read
#'     from the compensation change when present, else from a coarse role rank.}
#' }
#' Using both the raw and standardized titles is what catches the case where the
#' standardizer itself is inconsistent (two standard labels for one raw title, or
#' the reverse): agreement on *either* view marks the change as a relabel.
#'
#' @param df A linked panel: output of [link_panel()], carrying `EMP_ID`.
#' @param cols Column mapping; see [synthid_cols()] (uses `org_id`, `year`, `name`).
#' @param emp_id Person-cluster id column (default `"EMP_ID"`).
#' @param title_standard,title_raw Standardized and raw title columns. `title_raw`
#'   is optional; when absent the raw view is skipped and only the standardized
#'   title drives classification.
#' @param comp Optional compensation column (default `"tot.comp"`); when present it
#'   is the primary signal for promotion direction. Absent -> direction falls back
#'   to role rank.
#' @param min_sim Jaro-Winkler floor at which two titles count as "the same label"
#'   (default `0.85`). Higher = stricter (more changes judged real).
#' @param comp_delta Fractional compensation change treated as a real pay move
#'   (default `0.10`); within +/- this, pay is called flat and direction defers to
#'   role rank.
#' @param overrides Passed to [classify_title_role()] for the role columns.
#' @return A data frame of class `synthid_promotions`, one row per title change
#'   (all types), ordered by person then year:
#'   \describe{
#'     \item{`EMP_ID`, org id (named per `cols`), `name`}{The person.}
#'     \item{`from_year`, `to_year`}{Consecutive observed years across the change.}
#'     \item{`from_title`, `to_title`, `from_title_raw`, `to_title_raw`}{The
#'       standardized and raw labels on each side.}
#'     \item{`from_role`, `to_role`}{Coarse roles ([classify_title_role()]).}
#'     \item{`sim_standard`, `sim_raw`}{Jaro-Winkler similarity of the two labels.}
#'     \item{`from_comp`, `to_comp`, `comp_change`, `comp_pct`}{Compensation move
#'       (present only when `comp` is supplied).}
#'     \item{`change_type`}{Factor `relabel`/`oscillation`/`transient`/`real`.}
#'     \item{`direction`}{For `real` rows: `promotion`/`demotion`/`lateral`; `NA`
#'       otherwise.}
#'     \item{`basis`}{What set the direction: `"comp"`, `"role"`, or `"none"`.}
#'   }
#'   A run summary is attached as attribute `"synthid_promotions"`.
#' @examples
#' \dontrun{
#' linked <- link_panel(panel)
#' pr <- flag_promotions(linked)
#' subset(pr, change_type == "real" & direction == "promotion")
#' table(pr$change_type)                        # how much was noise
#' }
#' @seealso [flag_position_transitions()], [classify_title_role()].
#' @export
flag_promotions <- function(df, cols = synthid_cols(),
                            emp_id = "EMP_ID",
                            title_standard = "title.standard",
                            title_raw = "title.raw",
                            comp = "tot.comp",
                            min_sim = 0.85, comp_delta = 0.10,
                            overrides = NULL) {
  .check_linked(df, emp_id)
  for (nm in c(cols$year, title_standard)) {
    if (!nm %in% names(df)) {
      stop(sprintf("column '%s' not found in data frame.", nm), call. = FALSE)
    }
  }
  has_raw  <- !is.null(title_raw) && title_raw %in% names(df)
  has_comp <- !is.null(comp) && comp %in% names(df)

  id    <- as.character(df[[emp_id]])
  yr    <- suppressWarnings(as.integer(df[[cols$year]]))
  std   <- as.character(df[[title_standard]])
  raw   <- if (has_raw) as.character(df[[title_raw]]) else rep(NA_character_, nrow(df))
  org   <- if (cols$org_id %in% names(df)) as.character(df[[cols$org_id]]) else rep(NA_character_, nrow(df))
  name  <- if (cols$name %in% names(df)) as.character(df[[cols$name]]) else rep(NA_character_, nrow(df))
  cmp   <- if (has_comp) suppressWarnings(as.numeric(df[[comp]])) else rep(NA_real_, nrow(df))
  role  <- as.character(classify_title_role(std, overrides = overrides, levels = FALSE))
  nstd  <- .norm_pos(std)
  nraw  <- .norm_pos(raw)

  ok  <- !is.na(id) & !is.na(yr) & !is.na(nstd)
  idx <- split(which(ok), id[ok])

  ev <- list()
  for (rows in idx) {
    if (length(rows) < 2L) next
    ## One record per (person, year): keep the best-paid to break rare dup rows.
    o <- order(yr[rows], -ifelse(is.na(cmp[rows]), -Inf, cmp[rows]))
    rows <- rows[o]
    rows <- rows[!duplicated(yr[rows])]
    if (length(rows) < 2L) next

    ry <- yr[rows]; rs <- nstd[rows]; rr <- nraw[rows]
    for (t in 2:length(rows)) {
      if (identical(rs[t], rs[t - 1L])) next            # standardized title unchanged
      pr <- rows[t - 1L]; cu <- rows[t]

      sim_std <- .jw(rs[t - 1L], rs[t])
      sim_raw <- .jw(rr[t - 1L], rr[t])
      relabel <- (!is.na(sim_std) && sim_std >= min_sim) ||
                 (!is.na(sim_raw) && sim_raw >= min_sim)

      ## Upstream oscillation: the new label already appeared before the prior year.
      up <- rs[seq_len(t - 2L)]
      up <- up[!is.na(up)]
      osc <- length(up) > 0L &&
             any(stringdist::stringsim(up, rs[t], method = "jw") >= min_sim)

      ## Transient: novel label that reverts to the prior label next year.
      revert <- FALSE
      if (t < length(rows)) {
        nxt <- rs[t + 1L]
        revert <- isTRUE(.jw(nxt, rs[t - 1L]) >= min_sim) &&
                  !isTRUE(.jw(nxt, rs[t]) >= min_sim)
      }

      change_type <-
        if (relabel) "relabel"
        else if (osc) "oscillation"
        else if (revert) "transient"
        else "real"

      direction <- NA_character_; basis <- "none"
      if (change_type == "real") {
        cc  <- cmp[cu] - cmp[pr]
        cpc <- if (!is.na(cmp[pr]) && cmp[pr] > 0) cc / cmp[pr] else NA_real_
        if (has_comp && !is.na(cpc)) {
          basis <- "comp"
          direction <- if (cpc >= comp_delta) "promotion"
                       else if (cpc <= -comp_delta) "demotion" else "lateral"
        } else {
          rk_p <- .role_rank(role[pr]); rk_c <- .role_rank(role[cu])
          if (!is.na(rk_p) && !is.na(rk_c) && rk_p != rk_c) {
            basis <- "role"
            direction <- if (rk_c > rk_p) "promotion" else "demotion"
          } else {
            direction <- "lateral"
          }
        }
      }

      ev[[length(ev) + 1L]] <- data.frame(
        EMP_ID = id[cu], org = org[cu], name = name[cu],
        from_year = ry[t - 1L], to_year = ry[t],
        from_title = std[pr], to_title = std[cu],
        from_title_raw = raw[pr], to_title_raw = raw[cu],
        from_role = role[pr], to_role = role[cu],
        sim_standard = sim_std, sim_raw = sim_raw,
        from_comp = cmp[pr], to_comp = cmp[cu],
        comp_change = cmp[cu] - cmp[pr],
        comp_pct = if (!is.na(cmp[pr]) && cmp[pr] > 0) (cmp[cu] - cmp[pr]) / cmp[pr] else NA_real_,
        change_type = change_type, direction = direction, basis = basis,
        stringsAsFactors = FALSE
      )
    }
  }

  out <- if (length(ev)) do.call(rbind, ev) else data.frame(
    EMP_ID = character(0), org = character(0), name = character(0),
    from_year = integer(0), to_year = integer(0),
    from_title = character(0), to_title = character(0),
    from_title_raw = character(0), to_title_raw = character(0),
    from_role = character(0), to_role = character(0),
    sim_standard = numeric(0), sim_raw = numeric(0),
    from_comp = numeric(0), to_comp = numeric(0),
    comp_change = numeric(0), comp_pct = numeric(0),
    change_type = character(0), direction = character(0), basis = character(0),
    stringsAsFactors = FALSE
  )
  names(out)[names(out) == "org"] <- cols$org_id
  if (!has_comp) out[c("from_comp", "to_comp", "comp_change", "comp_pct")] <- NULL
  out$change_type <- factor(out$change_type,
                            levels = c("real", "transient", "oscillation", "relabel"))
  out <- out[order(out$EMP_ID, out$to_year), , drop = FALSE]
  rownames(out) <- NULL

  attr(out, "synthid_promotions") <- list(
    n_changes = nrow(out),
    by_type   = table(out$change_type),
    n_real    = sum(out$change_type == "real", na.rm = TRUE),
    by_dir    = table(out$direction[out$change_type == "real"])
  )
  class(out) <- c("synthid_promotions", "data.frame")
  out
}

#' @export
print.synthid_promotions <- function(x, ...) {
  r <- attr(x, "synthid_promotions")
  if (is.null(r)) { NextMethod(); return(invisible(x)) }  # e.g. after column subsetting
  cat("synthid promotions\n------------------\n")
  cat(sprintf("title changes   : %d\n", r$n_changes %||% 0L))
  if (!is.null(r$by_type)) {
    cat("by change_type:\n")
    for (nm in names(r$by_type)) cat(sprintf("  %-12s %d\n", nm, r$by_type[[nm]]))
  }
  if (!is.null(r$by_dir) && length(r$by_dir)) {
    cat("real, by direction:\n")
    for (nm in names(r$by_dir)) cat(sprintf("  %-12s %d\n", nm, r$by_dir[[nm]]))
  }
  NextMethod()
}

## ---- 3. position churn ------------------------------------------------------

#' Measure churn within a position over time
#'
#' Turnover accounting for a position -- most often a board -- across an
#' organization's filing years. For each year it reports how many distinct people
#' held the position, and, relative to the prior filing year, how many are new, how
#' many have left, and how many are stable (held it both years). This is the
#' body-level complement to [flag_position_transitions()]: churn counts a whole
#' set's turnover, transitions name the individual handovers within a one- or
#' two-seat post.
#'
#' The prior filing year is the organization's own previous year in `df`, so a year
#' in which the position falls to zero holders (a board that skipped reporting, say)
#' still produces a row -- with every prior holder counted as `n_left`. Rows begin
#' at each group's first year with a holder; earlier empty years are dropped.
#'
#' @param df A linked panel: output of [link_panel()], carrying the id column.
#' @param position Character vector of standardized titles (or `NULL`).
#' @param role Character vector of coarse roles -- `"BOARD"`, `"OFFICER"`,
#'   `"STAFF"`, `"OTHER"` (or `NULL`). For a board pass `role = "BOARD"`. At least
#'   one of `position`/`role` is required.
#' @param match How `position` is matched: `"exact"` (default), `"regex"`, or
#'   `"fuzzy"` (Jaro-Winkler at `min_sim`).
#' @param min_sim Similarity floor for `match = "fuzzy"`.
#' @param overrides Passed to [classify_title_role()] when selecting by `role`.
#' @param cols Column mapping; see [synthid_cols()] (uses `org_id`, `year`).
#' @param id Person id column to count (default `"EMP_ID"`). Pass a cross-org id to
#'   pool churn across organizations with `by_org = FALSE`.
#' @param title Standardized-title column (default `"title.standard"`).
#' @param by_org If `TRUE` (default) churn is computed per organization; if `FALSE`
#'   it is pooled over the whole panel (population churn for the position).
#' @return A data frame of class `synthid_churn`, one row per group-year (a group is
#'   an organization when `by_org`, else the whole panel):
#'   \describe{
#'     \item{org id (when `by_org`), year (named per `cols`)}{The group and year.}
#'     \item{`n_current`}{Distinct ids holding the position that year.}
#'     \item{`n_new`}{Holders not present the prior filing year (`NA` in the first).}
#'     \item{`n_left`}{Prior-year holders absent this year (`NA` in the first).}
#'     \item{`n_stable`}{Holders present both years (`NA` in the first).}
#'     \item{`n_prev`}{Holders the prior filing year.}
#'     \item{`retention`, `turnover`}{`n_stable / n_prev` and `n_new / n_current`.}
#'   }
#'   A run summary is attached as attribute `"synthid_churn"`.
#' @examples
#' \dontrun{
#' linked <- link_panel(panel)
#' position_churn(linked, role = "BOARD")               # board churn per org
#' position_churn(linked, position = "CEO", by_org = FALSE)  # pooled CEO churn
#' }
#' @seealso [flag_position_transitions()], [flag_promotions()].
#' @export
position_churn <- function(df, position = NULL, role = NULL,
                           cols = synthid_cols(),
                           id = "EMP_ID",
                           title = "title.standard",
                           by_org = TRUE,
                           match = c("exact", "regex", "fuzzy"),
                           min_sim = 0.90, overrides = NULL) {
  match <- match.arg(match)
  .check_linked(df, id)
  for (nm in c(cols$org_id, cols$year)) {
    if (by_org && !nm %in% names(df)) {
      stop(sprintf("column '%s' not found in data frame.", nm), call. = FALSE)
    }
  }
  if (!cols$year %in% names(df)) {
    stop(sprintf("column '%s' not found in data frame.", cols$year), call. = FALSE)
  }

  grp <- if (by_org) as.character(df[[cols$org_id]]) else rep("ALL", nrow(df))
  yr  <- suppressWarnings(as.integer(df[[cols$year]]))
  eid <- as.character(df[[id]])
  hold <- .match_position(df, position, role, title, match, min_sim, overrides)

  ok <- !is.na(grp) & !is.na(yr) & !is.na(eid)
  ## Group filing timeline (all years the group appears), independent of the seat.
  grp_years <- lapply(split(yr[ok], grp[ok]), function(v) sort(unique(v)))

  keep <- ok & hold
  gy_key <- function(g, y) paste(g, y, sep = "\r")
  hset <- lapply(split(eid[keep], gy_key(grp[keep], yr[keep])), unique)

  rows <- list()
  for (g in names(grp_years)) {
    ys <- grp_years[[g]]
    have <- vapply(ys, function(y) length(hset[[gy_key(g, y)]]) > 0L, logical(1))
    if (!any(have)) next
    start <- min(which(have))
    for (k in start:length(ys)) {
      y   <- ys[k]
      cur <- hset[[gy_key(g, y)]]; cur <- if (is.null(cur)) character(0) else cur
      if (k > start) {
        py   <- ys[k - 1L]
        prev <- hset[[gy_key(g, py)]]; prev <- if (is.null(prev)) character(0) else prev
        n_new <- length(setdiff(cur, prev))
        n_left <- length(setdiff(prev, cur))
        n_stable <- length(intersect(cur, prev))
        n_prev <- length(prev)
      } else {
        n_new <- n_left <- n_stable <- NA_integer_
        n_prev <- NA_integer_
      }
      rows[[length(rows) + 1L]] <- data.frame(
        grp = g, yr = y, n_current = length(cur),
        n_new = n_new, n_left = n_left, n_stable = n_stable, n_prev = n_prev,
        retention = if (!is.na(n_prev) && n_prev > 0) n_stable / n_prev else NA_real_,
        turnover  = if (length(cur) > 0 && !is.na(n_new)) n_new / length(cur) else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }

  out <- if (length(rows)) do.call(rbind, rows) else data.frame(
    grp = character(0), yr = integer(0), n_current = integer(0),
    n_new = integer(0), n_left = integer(0), n_stable = integer(0),
    n_prev = integer(0), retention = numeric(0), turnover = numeric(0),
    stringsAsFactors = FALSE
  )
  if (by_org) {
    names(out)[1:2] <- c(cols$org_id, cols$year)
    out <- out[order(out[[cols$org_id]], out[[cols$year]]), , drop = FALSE]
  } else {
    out$grp <- NULL
    names(out)[1] <- cols$year
    out <- out[order(out[[cols$year]]), , drop = FALSE]
  }
  rownames(out) <- NULL

  attr(out, "synthid_churn") <- list(
    by_org  = by_org,
    n_rows  = nrow(out),
    n_groups = if (by_org) length(unique(out[[cols$org_id]])) else 1L,
    total_new  = sum(out$n_new, na.rm = TRUE),
    total_left = sum(out$n_left, na.rm = TRUE)
  )
  class(out) <- c("synthid_churn", "data.frame")
  out
}

#' @export
print.synthid_churn <- function(x, ...) {
  r <- attr(x, "synthid_churn")
  if (is.null(r)) { NextMethod(); return(invisible(x)) }  # e.g. after column subsetting
  cat("synthid position churn\n----------------------\n")
  cat(sprintf("scope           : %s\n", if (isTRUE(r$by_org)) "per organization" else "pooled panel"))
  cat(sprintf("group-year rows : %d\n", r$n_rows %||% 0L))
  cat(sprintf("groups          : %d\n", r$n_groups %||% 0L))
  cat(sprintf("arrivals / departures (all periods): %d / %d\n",
              r$total_new %||% 0L, r$total_left %||% 0L))
  NextMethod()
}
