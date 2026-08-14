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
#' @keywords internal
"_PACKAGE"

## Quiet R CMD check notes about non-standard evaluation used with dplyr/data.table.
utils::globalVariables(c(
  ".", ".row_uid", ".emp_cluster", "EMP_ID", "ein", "taxyr", "score",
  "surname_weight", "n", "cluster", "component", "member", "yr_x", "yr_y",
  "row_x", "row_y", ".x", ".y", "Var1", "Var2"
))
