#' Assign stable cross-year person identifiers to a compensation panel
#'
#' End-to-end pipeline: prepare the panel, link records across every pair of tax
#' years within each organization, resolve the accepted links into person
#' clusters (one record per organization-year, enforced), and stamp one
#' deterministic `EMP_ID` on every record.
#'
#' @param df A stacked multi-year panel: the parsed output of \pkg{titleclassifier}
#'   and \pkg{peopleparser}. Must contain at least the organization id, tax year,
#'   and full name columns named by `cols`.
#' @param cols Column mapping; see [synthid_cols()].
#' @param weights Named base match weights; see [default_weights()].
#' @param threshold Minimum match score to link two records when
#'   `method = "weighted"` (default `7`, chosen by tuning against a labeled slice;
#'   see `dev/tune_threshold.R`).
#' @param method Scoring method: `"weighted"` (default) uses the hand-set
#'   Fellegi-Sunter-style additive score; `"em"` fits an unsupervised
#'   Fellegi-Sunter latent-class model ([fit_match_model()]) to the candidate
#'   comparisons and links by the learned posterior match probability, also
#'   attaching a per-person link confidence.
#' @param prob_threshold Minimum posterior match probability to link two records
#'   when `method = "em"` (default `0.5`).
#' @param verbose Print progress.
#' @return `df` with added columns: `EMP_ID` (stable person id), `EMP_N_RECORDS`,
#'   `EMP_N_YEARS`, and -- for `method = "em"` -- `EMP_LINK_CONF` (the weakest
#'   posterior probability among the links holding the person's cluster together;
#'   `NA` for single-record persons), a ready-made confidence input for a
#'   downstream panel model. Diagnostics are attached as the `"synthid"` attribute
#'   (see [synthid_report()]).
#' @examples
#' \dontrun{
#' panel <- readr::read_csv("PANEL-2019-2021.csv")
#' linked <- link_panel(panel)                 # hand-weighted
#' linked_em <- link_panel(panel, method = "em")
#' fs_weights(attr(linked_em, "synthid")$model) # learned weights
#' }
#' @export
link_panel <- function(df, cols = synthid_cols(),
                       weights = default_weights(),
                       threshold = 7, method = c("weighted", "em"),
                       prob_threshold = 0.5, verbose = FALSE) {
  stopifnot(is.data.frame(df))
  method <- match.arg(method)
  if (verbose) message("Preparing panel (", nrow(df), " records)...")
  prep <- prepare_panel(df, cols)
  work <- prep$work

  if (anyDuplicated(work$.row_uid)) {
    stop("Internal error: record keys are not unique.", call. = FALSE)
  }

  model <- NULL
  edge_conf <- NULL
  if (method == "weighted") {
    if (verbose) message("Linking across year pairs (weighted score)...")
    edges <- link_year_pairs(work, weights = weights,
                             threshold = threshold, verbose = verbose)
  } else {
    if (verbose) message("Fitting EM model and linking by posterior...")
    cmp <- candidate_comparisons(df, cols)
    model <- fit_match_model(cmp, method = "em")
    cmp$.p <- predict_match(model, cmp)
    edges <- link_edges_by_value(cmp, cmp$.p, prob_threshold)
    edge_conf <- edges$score  # posterior probability of each accepted link
  }

  if (verbose) message("Resolving clusters...")
  cl <- resolve_clusters(work, edges)
  emp <- assign_emp_ids(cl$assignment)

  ## Align cluster output back to the original row order.
  emp <- emp[match(work$.row_uid, emp$.row_uid), , drop = FALSE]
  size <- stats::ave(seq_len(nrow(emp)), emp$EMP_ID, FUN = length)
  nyr <- stats::ave(as.character(work$.year), emp$EMP_ID,
                    FUN = function(v) length(unique(v)))

  out <- df
  out$EMP_ID <- emp$EMP_ID
  out$EMP_N_RECORDS <- as.integer(size)
  out$EMP_N_YEARS <- as.integer(nyr)

  if (method == "em") {
    ## weakest link probability within each cluster -> per-record confidence.
    ## Only count links whose endpoints actually ended up in the same cluster
    ## (an edge dropped by the one-per-org-year guard did not form the cluster).
    cx <- emp$EMP_ID[match(edges$row_x, emp$.row_uid)]
    cy <- emp$EMP_ID[match(edges$row_y, emp$.row_uid)]
    used <- !is.na(cx) & cx == cy
    conf <- tapply(edges$score[used], cx[used], min)
    out$EMP_LINK_CONF <- as.numeric(conf[out$EMP_ID])
  }

  n_clusters <- length(unique(emp$EMP_ID))
  report <- list(
    n_records      = nrow(df),
    n_orgs         = length(unique(work$.org)),
    n_years        = length(unique(work$.year)),
    years          = sort(unique(as.character(work$.year))),
    n_clusters     = n_clusters,
    n_singletons   = sum(size == 1L),
    n_multi_year   = sum(!duplicated(emp$EMP_ID) & size > 1L),
    n_links        = nrow(edges),
    rejected_edges = cl$rejected_edges,
    method         = method,
    threshold      = if (method == "weighted") threshold else prob_threshold,
    weights        = weights,
    model          = model
  )
  attr(out, "synthid") <- report
  out
}

#' Greedy one-to-one links from a scored candidate table
#'
#' Thresholds a candidate table on `value`, then applies greedy one-to-one
#' selection within each organization-year-pair. Shared by the `method = "em"`
#' path of [link_panel()].
#'
#' @param cand Candidate table with `row_x`, `row_y`, `org`, `yr_x`, `yr_y`.
#' @param value Numeric vector (same length as `cand` rows) to threshold and rank.
#' @param threshold Minimum `value` to keep.
#' @return Accepted edges: `row_x`, `row_y`, `score` (the kept `value`).
#' @keywords internal
link_edges_by_value <- function(cand, value, threshold) {
  keep <- which(!is.na(value) & value >= threshold)
  if (length(keep) == 0L) {
    return(data.frame(row_x = character(0), row_y = character(0),
                      score = numeric(0), stringsAsFactors = FALSE))
  }
  sub <- data.frame(row_x = cand$row_x[keep], row_y = cand$row_y[keep],
                    score = value[keep],
                    grp = paste(cand$org[keep], cand$yr_x[keep], cand$yr_y[keep]),
                    stringsAsFactors = FALSE)
  acc <- lapply(split(sub, sub$grp), function(g)
    greedy_one_to_one(g[c("row_x", "row_y", "score")]))
  do.call(rbind, acc)
}

#' Print a summary of a linkage run
#'
#' @param df The data frame returned by [link_panel()].
#' @return `df`, invisibly.
#' @export
synthid_report <- function(df) {
  r <- attr(df, "synthid")
  if (is.null(r)) {
    stop("No 'synthid' report attached; run link_panel() first.", call. = FALSE)
  }
  cat("synthid linkage report\n")
  cat("----------------------\n")
  cat(sprintf("records            : %d\n", r$n_records))
  cat(sprintf("organizations      : %d\n", r$n_orgs))
  cat(sprintf("years              : %s\n", paste(r$years, collapse = ", ")))
  cat(sprintf("person clusters    : %d\n", r$n_clusters))
  cat(sprintf("  multi-year       : %d\n", r$n_multi_year))
  cat(sprintf("  singletons       : %d\n", r$n_singletons))
  cat(sprintf("accepted links     : %d\n", r$n_links))
  cat(sprintf("collisions blocked : %d\n", r$rejected_edges))
  meth <- r$method %||% "weighted"
  if (identical(meth, "em")) {
    cat(sprintf("method             : em (posterior >= %g)\n", r$threshold))
    if (!is.null(r$model)) cat(sprintf("prior match rate p : %.4f\n", r$model$p))
  } else {
    cat(sprintf("method             : weighted (score >= %g)\n", r$threshold))
  }
  compression <- 1 - r$n_clusters / r$n_records
  cat(sprintf("panel compression  : %.1f%% (records -> persons)\n",
              100 * compression))
  invisible(df)
}
