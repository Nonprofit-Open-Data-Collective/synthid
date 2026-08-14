#' synthid: Cross-Year Person Linkage and Synthetic ID Assignment
#'
#' Turns a stacked multi-year panel of Form 990 compensation records (the parsed
#' output of \pkg{titleclassifier} and \pkg{peopleparser}) into a panel keyed by a
#' stable per-person identifier. The pipeline is:
#' \enumerate{
#'   \item [build_id_string()] / [create_emp_ids()] -- a canonical, human-readable
#'     record string and a deterministic hash used as the per-record key.
#'   \item [link_year_pairs()] -- generate candidate pairs blocked on organization,
#'     compare name components, score with a within-organization surname-frequency
#'     adjustment, and select one-to-one matches within each ordered year pair.
#'   \item [resolve_clusters()] -- close the accepted links into connected
#'     components, split any cluster that would place two people from the same
#'     organization-year together, and stamp one stable `EMP_ID` per cluster.
#'   \item [link_panel()] -- the end-to-end orchestrator over the three steps.
#' }
#'
#' @section Reviewing a linkage:
#' A separate triage layer audits the output of [link_panel()] and feeds the
#' upstream name parser. It does not change any `EMP_ID`; it only surfaces
#' records worth a second look.
#' \itemize{
#'   \item [flag_links()] scores every multi-record person on cross-year name
#'     consistency and sorts each questionable cluster into one primary
#'     `category` (nickname/spelling, compound surname, field-order swap, a
#'     parser artifact, or the residual `review`). [link_review_queue()] returns
#'     just the `review` cases -- the genuinely uncertain links.
#'   \item [parse_fail_log()] scans records for recoverable *parser* defects
#'     (a honorific fused to the given name, credentials leaked into the surname,
#'     a stray initial glued to the surname) and emits one `input -> wrong parse
#'     -> expected` row each, usable directly as \pkg{peopleparser} regression
#'     fixtures. [parse_fail_tokens()] rolls that log up into the small list of
#'     candidate title/credential tokens to add upstream.
#' }
#' See `dev/generate_link_review.R` for the end-to-end review + handoff run.
#'
#' @keywords internal
"_PACKAGE"

## Quiet R CMD check notes about non-standard evaluation used with dplyr/data.table.
utils::globalVariables(c(
  ".", ".row_uid", ".emp_cluster", "EMP_ID", "ein", "taxyr", "score",
  "surname_weight", "n", "cluster", "component", "member", "yr_x", "yr_y",
  "row_x", "row_y", ".x", ".y", "Var1", "Var2"
))
