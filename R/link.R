#' Standard column names for a titleclassifier/peopleparser panel
#'
#' The default mapping from logical roles to the column names produced by the
#' upstream packages. Pass a modified copy to [link_panel()] to adapt to a panel
#' with different column names.
#'
#' @return A named list of column names.
#' @export
synthid_cols <- function() {
  list(
    org_id   = "ein",
    org_name = "org.name",
    year     = "taxyr",
    name     = "name",
    features = c("salutation", "first_name", "middle_name",
                 "last_name", "suffix", "gender", "title.standard")
  )
}

#' Prepare a raw panel for linkage
#'
#' Validates required columns, normalizes name/attribute fields (upper-cases and
#' trims), converts blanks in optional fields to `NA` so absence is treated as
#' neutral rather than as agreement, maps an unknown gender (`"U"`) to `NA`, and
#' attaches a unique per-record key (`.row_uid`) plus the per-record surname
#' weight.
#'
#' @param df Raw panel data frame.
#' @param cols Column mapping (see [synthid_cols()]).
#' @return A list with `work` (the cleaned working frame, including `.row_uid` and
#'   `.surname_w`) and `id_string` (the human-readable per-record string).
#' @keywords internal
prepare_panel <- function(df, cols = synthid_cols()) {
  required <- c(cols$org_id, cols$year, cols$name)
  miss <- setdiff(required, names(df))
  if (length(miss)) {
    stop("prepare_panel(): missing required column(s): ",
         paste(miss, collapse = ", "), call. = FALSE)
  }
  feats <- intersect(cols$features, names(df))
  dropped <- setdiff(cols$features, names(df))
  if (length(dropped)) {
    warning("Ignoring feature(s) not present in data: ",
            paste(dropped, collapse = ", "), call. = FALSE)
  }

  work <- data.frame(
    .org  = as.character(df[[cols$org_id]]),
    .year = df[[cols$year]],
    .name = toupper(trimws(as.character(df[[cols$name]]))),
    stringsAsFactors = FALSE
  )
  for (f in feats) {
    v <- toupper(trimws(as.character(df[[f]])))
    v[v == ""] <- NA
    work[[f]] <- v
  }
  ## Absence should be neutral, not a token that can "agree".
  for (f in intersect(c("middle_name", "suffix", "salutation"), feats)) {
    work[[f]][is.na(work[[f]])] <- NA
  }
  if ("gender" %in% feats) {
    work$gender[work$gender %in% c("U", "UNKNOWN", "")] <- NA
  }

  id_string <- build_id_string(
    df, org_id = cols$org_id, year = cols$year,
    name = cols$name,
    title = if ("title.standard" %in% names(df)) "title.standard" else cols$name
  )
  work$.row_uid <- id_string
  work$.surname_w <- surname_weight(
    data.frame(ein = work$.org, name = work$.name,
               last_name = if ("last_name" %in% names(work)) work$last_name else work$.name,
               stringsAsFactors = FALSE),
    org_id = "ein", name = "name", last_name = "last_name"
  )

  list(work = work, id_string = id_string, features = feats)
}

#' Build the per-field comparator list for [reclin2::compare_pairs()]
#'
#' @param feats Feature (column) names being compared.
#' @return A named list of comparator functions (surname and categorical fields);
#'   fields not named here fall back to the default Jaro-Winkler comparator.
#' @keywords internal
match_comparators <- function(feats) {
  comparators <- list()
  if ("last_name" %in% feats) comparators[["last_name"]] <- compare_last_names
  ## The nickname/phonetic first-name comparator can be disabled (falling back to
  ## the default Jaro-Winkler) via options(synthid.first_name_comparator = FALSE),
  ## mainly to A/B its effect.
  if ("first_name" %in% feats &&
      isTRUE(getOption("synthid.first_name_comparator", TRUE))) {
    comparators[["first_name"]] <- compare_first_names
  }
  for (f in intersect(c("gender", "suffix", "salutation"), feats)) {
    comparators[[f]] <- cmp_exact
  }
  comparators
}

#' Score every candidate pair between two single-year frames
#'
#' Blocks on organization, compares the configured fields, and returns the
#' surname-frequency-adjusted score for *every* candidate pair (no threshold, no
#' selection). Shared by [link_year_pairs()] and [candidate_scores()] so the two
#' can never drift apart.
#'
#' @param dx,dy Prepared single-year frames (subsets of a [prepare_panel()]
#'   `work` frame).
#' @param feats Feature column names to compare.
#' @param comparators Comparator list from [match_comparators()].
#' @param weights Named base weights.
#' @return A data frame with `ix`, `iy` (row indices into `dx`, `dy`) and `score`,
#'   or `NULL` if there are no candidate pairs.
#' @keywords internal
score_year_pair <- function(dx, dy, feats, comparators, weights) {
  if (nrow(dx) == 0L || nrow(dy) == 0L) return(NULL)
  pairs <- reclin2::pair_blocking(dx, dy, on = ".org")
  if (nrow(pairs) == 0L) return(NULL)
  pairs <- reclin2::compare_pairs(
    pairs, on = feats,
    comparators = comparators,
    default_comparator = reclin2::cmp_jarowinkler(),
    inplace = FALSE
  )
  pdf <- as.data.frame(pairs)
  sw_x <- dx$.surname_w[pdf$.x]
  sw_y <- dy$.surname_w[pdf$.y]
  data.frame(ix = pdf$.x, iy = pdf$.y,
             score = score_pairs(pdf, weights, sw_x, sw_y))
}

#' Link records across every pair of years within organizations
#'
#' For each ordered pair of tax years, blocks candidate pairs on organization,
#' compares the configured name fields, scores each candidate with the
#' surname-frequency adjustment, keeps candidates at or above `threshold`, and
#' enforces a one-to-one assignment within the year pair (greedy, highest score
#' first). Comparing all year pairs -- not just consecutive ones -- lets a person
#' absent in the middle year still be linked across the gap.
#'
#' @param work Prepared working frame from [prepare_panel()].
#' @param weights Named base weights (see [default_weights()]).
#' @param threshold Minimum score for a candidate to be eligible.
#' @param verbose Print per-year-pair progress.
#' @return A data frame of accepted edges: `row_x`, `row_y` (the `.row_uid`s),
#'   `score`, `yr_x`, `yr_y`.
#' @keywords internal
link_year_pairs <- function(work, weights = default_weights(),
                            threshold = 7, verbose = FALSE) {
  feats <- intersect(names(weights), names(work))
  comparators <- match_comparators(feats)
  years <- sort(unique(work$.year))
  edges <- vector("list", 0L)

  for (i in seq_along(years)) {
    for (j in seq_along(years)) {
      if (j <= i) next
      yx <- years[i]
      yy <- years[j]
      dx <- work[work$.year == yx, , drop = FALSE]
      dy <- work[work$.year == yy, , drop = FALSE]
      sc <- score_year_pair(dx, dy, feats, comparators, weights)
      if (is.null(sc)) next

      keep <- which(!is.na(sc$score) & sc$score >= threshold)
      if (length(keep) == 0L) next
      cand <- data.frame(
        row_x = dx$.row_uid[sc$ix[keep]],
        row_y = dy$.row_uid[sc$iy[keep]],
        score = sc$score[keep],
        stringsAsFactors = FALSE
      )
      accepted <- greedy_one_to_one(cand)
      accepted$yr_x <- yx
      accepted$yr_y <- yy
      edges[[length(edges) + 1L]] <- accepted
      if (verbose) {
        message(sprintf("  %s-%s: %d candidates -> %d one-to-one links",
                        yx, yy, length(keep), nrow(accepted)))
      }
    }
  }
  if (length(edges) == 0L) {
    return(data.frame(row_x = character(0), row_y = character(0),
                      score = numeric(0), yr_x = character(0),
                      yr_y = character(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, edges)
}

#' Score all candidate cross-year pairs (for evaluation and threshold tuning)
#'
#' Returns every candidate pair the linker would consider -- blocked on
#' organization, across all year pairs -- with its match score and the raw name
#' fields of both sides, but without applying any threshold or one-to-one
#' selection. This is the input to threshold tuning and labeling.
#'
#' @param df A raw panel (as for [link_panel()]).
#' @param cols Column mapping; see [synthid_cols()].
#' @param weights Named base weights; see [default_weights()].
#' @param fields Raw fields to attach for each side (suffixed `_x` / `_y`).
#' @return A data frame, one row per candidate pair: `row_x`, `row_y`, `yr_x`,
#'   `yr_y`, `org`, `score`, `surname_w_x`, `surname_w_y`, and the requested
#'   `fields` for each side.
#' @examples
#' \dontrun{
#' cs <- candidate_scores(panel)
#' hist(cs$score)
#' }
#' @export
candidate_scores <- function(df, cols = synthid_cols(),
                             weights = default_weights(),
                             fields = c("name", "first_name", "middle_name",
                                        "last_name", "suffix", "gender")) {
  prep <- prepare_panel(df, cols)
  work <- prep$work
  feats <- intersect(names(weights), names(work))
  comparators <- match_comparators(feats)
  fields <- intersect(fields, names(work))
  years <- sort(unique(work$.year))
  out <- vector("list", 0L)

  for (i in seq_along(years)) {
    for (j in seq_along(years)) {
      if (j <= i) next
      dx <- work[work$.year == years[i], , drop = FALSE]
      dy <- work[work$.year == years[j], , drop = FALSE]
      sc <- score_year_pair(dx, dy, feats, comparators, weights)
      if (is.null(sc)) next
      rec <- data.frame(
        row_x = dx$.row_uid[sc$ix], row_y = dy$.row_uid[sc$iy],
        yr_x = years[i], yr_y = years[j],
        org = dx$.org[sc$ix], score = sc$score,
        surname_w_x = dx$.surname_w[sc$ix],
        surname_w_y = dy$.surname_w[sc$iy],
        stringsAsFactors = FALSE
      )
      for (f in fields) {
        rec[[paste0(f, "_x")]] <- dx[[f]][sc$ix]
        rec[[paste0(f, "_y")]] <- dy[[f]][sc$iy]
      }
      out[[length(out) + 1L]] <- rec
    }
  }
  if (length(out) == 0L) return(data.frame())
  do.call(rbind, out)
}

#' Greedy one-to-one selection among candidate edges
#'
#' Sorts candidate edges by descending score and accepts an edge only if neither
#' of its endpoints has already been claimed, yielding a one-to-one matching
#' within a single year pair.
#'
#' @param cand Data frame with `row_x`, `row_y`, `score`.
#' @return The accepted subset of `cand`.
#' @keywords internal
greedy_one_to_one <- function(cand) {
  ord <- order(-cand$score)
  used_x <- new.env(parent = emptyenv())
  used_y <- new.env(parent = emptyenv())
  take <- logical(nrow(cand))
  for (k in ord) {
    ax <- cand$row_x[k]
    ay <- cand$row_y[k]
    if (is.null(used_x[[ax]]) && is.null(used_y[[ay]])) {
      take[k] <- TRUE
      used_x[[ax]] <- TRUE
      used_y[[ay]] <- TRUE
    }
  }
  cand[take, , drop = FALSE]
}
