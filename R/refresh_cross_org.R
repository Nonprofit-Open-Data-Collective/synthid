#' Diff two cross-org assignments
#'
#' Compares a prior and a refreshed cross-org assignment (both carrying `EMP_ID`,
#' `XORG_ID`, `XORG_N_ORGS`) and summarizes what a new wave changed. Because
#' `XORG_ID` is now anchored ([anchor_xorg_id()]), a cluster keeps its id as it
#' grows, so ids compare directly across the two runs.
#'
#' @param prior,new Cross-org assignment data frames (the `assignment` element of
#'   a [link_cross_org()] result).
#' @return A list: `n_new_persons` (persons in `new` absent from `prior` -- the
#'   wave arrivals), `newly_interlocking_persons` (returning persons whose org
#'   span rose from <=1 to >1), `persons_with_grown_span` (returning persons whose
#'   `XORG_N_ORGS` increased at all), `new_interlock_clusters` (multi-org
#'   `XORG_ID`s not multi-org before), and the interlock counts before/after.
#' @seealso [refresh_cross_org()]
#' @keywords internal
diff_cross_org <- function(prior, new) {
  pspan <- stats::setNames(as.integer(prior$XORG_N_ORGS), prior$EMP_ID)
  nspan <- stats::setNames(as.integer(new$XORG_N_ORGS), new$EMP_ID)

  new_persons <- setdiff(new$EMP_ID, prior$EMP_ID)
  both <- intersect(new$EMP_ID, prior$EMP_ID)
  grew <- both[nspan[both] > pspan[both]]
  newly_interlocking <- both[pspan[both] <= 1L & nspan[both] > 1L]

  prior_multi <- unique(prior$XORG_ID[prior$XORG_N_ORGS > 1L])
  new_multi   <- unique(new$XORG_ID[new$XORG_N_ORGS > 1L])

  list(
    n_new_persons = length(new_persons),
    newly_interlocking_persons = newly_interlocking,
    persons_with_grown_span = grew,
    new_interlock_clusters = setdiff(new_multi, prior_multi),
    n_interlocking_before = sum(prior$XORG_N_ORGS > 1L),
    n_interlocking_after  = sum(new$XORG_N_ORGS > 1L)
  )
}

#' Refresh cross-organization interlocks after integrating a new wave
#'
#' A new wave ([link_incremental()]) can create fresh cross-org interlocks -- a
#' new person who sits on a second org's board, or a returning person who picks up
#' an outside seat. This re-runs the cross-org linkage ([link_cross_org()]) over
#' the grown panel and, when given the previous assignment, reports exactly what
#' changed. Because `XORG_ID` is anchored to each interlock's founding member
#' ([anchor_xorg_id()]), an unchanged interlock keeps its id across refreshes;
#' only genuinely new or grown clusters move.
#'
#' This is a full re-link of the merged panel (cheap: the cross-org stage is
#' hash-blocked, not all-pairs), not a fully-incremental cross-org matcher -- a
#' later optimization would score only the wave-affected profiles against the
#' rest. For now the diff makes the full re-run's *effect* incremental to read.
#'
#' @param merged The grown, linked panel: `existing` with the wave's `new_stamped`
#'   rows appended (each row carries `EMP_ID`). See [link_incremental()].
#' @param prior Optional previous cross-org result to diff against: either the
#'   `assignment` data frame or a whole [link_cross_org()] result list. `NULL`
#'   skips the diff.
#' @param cols Column mapping; see [synthid_cols()].
#' @param state,ntee Optional geography / industry columns for tighter blocking;
#'   see [link_cross_org()].
#' @param threshold,max_block_size,weights,verbose Passed to [link_cross_org()].
#' @return A list: `xorg` (the full [link_cross_org()] result), `assignment` (its
#'   `EMP_ID` -> `XORG_ID` map, for convenience), and `changes` (the
#'   [diff_cross_org()] summary, or `NULL` when `prior` is not supplied).
#' @examples
#' \dontrun{
#' xo0 <- link_cross_org(existing, state = "state", ntee = "ntee")   # before
#' inc <- link_incremental(existing, wave)
#' merged <- rbind(existing[names(inc$new_stamped)], inc$new_stamped)
#' ref <- refresh_cross_org(merged, prior = xo0, state = "state", ntee = "ntee")
#' ref$changes$newly_interlocking_persons     # who spans a new org after the wave
#' }
#' @seealso [link_incremental()], [link_cross_org()]
#' @export
refresh_cross_org <- function(merged, prior = NULL, cols = synthid_cols(),
                              state = NULL, ntee = NULL,
                              weights = default_weights(), threshold = 7,
                              max_block_size = 5000L, verbose = FALSE) {
  stopifnot(is.data.frame(merged))
  if (!"EMP_ID" %in% names(merged)) {
    stop("refresh_cross_org(): `merged` must carry EMP_ID (link it first).",
         call. = FALSE)
  }
  xo <- link_cross_org(merged, cols = cols, state = state, ntee = ntee,
                       weights = weights, threshold = threshold,
                       max_block_size = max_block_size, verbose = verbose)

  changes <- NULL
  if (!is.null(prior)) {
    prior_assignment <- if (is.data.frame(prior)) prior
                        else if (is.list(prior) && !is.null(prior$assignment)) prior$assignment
                        else stop("refresh_cross_org(): `prior` must be an assignment ",
                                  "data frame or a link_cross_org() result list.",
                                  call. = FALSE)
    changes <- diff_cross_org(prior_assignment, xo$assignment)
  }

  list(xorg = xo, assignment = xo$assignment, changes = changes)
}
