#' Filter cross-organization edges by the role composition of their endpoints
#'
#' Selects cross-org candidate/accepted edges ([score_candidate_pairs()],
#' [link_cross_org()]) by the coarse role ([classify_title_role()]) each endpoint
#' held. Each edge endpoint is one within-org `EMP_ID` -- one person's whole
#' tenure at one organization -- so an endpoint's role evidence is the *set* of
#' roles it held across the years of that tenure (`profiles$roles`). This is a
#' within-tenure union, **not** a union across organizations (the other org is the
#' other endpoint).
#'
#' Because a within-tenure role set can mix roles (a paid director who later took
#' a board seat), `mode` controls how the set is reduced to a yes/no per endpoint:
#' \describe{
#'   \item{`any`}{Endpoint matches if it held *any* role in `target` (the
#'     union/"ever" behaviour -- same as `profiles$is_board` for `target="BOARD"`).}
#'   \item{`all`}{Endpoint matches only if *every* role it held is in `target`.}
#'   \item{`primary`}{Test only the modal role (`profiles$primary_role`).}
#' }
#' `negate = TRUE` flips the per-endpoint test (matches endpoints *not* in
#' `target`); combined with `mode = "any"` it means "never held a `target` role",
#' the strict complement. `match` then requires the test to hold for `both`
#' endpoints or `either`. An endpoint with no recorded role is treated as not in
#' `target`.
#'
#' @param edges Edge table with `emp_a`, `emp_b` (e.g. `link_cross_org()$edges`).
#' @param profiles Profiles those ids index ([build_person_profile()]); needs
#'   `roles` and `primary_role`.
#' @param target Role class(es) to test against, from `BOARD`/`OFFICER`/`STAFF`/
#'   `OTHER`.
#' @param mode How to reduce an endpoint's role set: `"any"`, `"all"`, `"primary"`.
#' @param match Require the endpoint test on `"both"` ends or `"either"`.
#' @param negate Match endpoints *not* satisfying the target test.
#' @param annotate Append `roles_a`, `roles_b` (the endpoints' role sets, `+`-
#'   joined) to the result for inspection.
#' @return The subset of `edges` whose endpoints satisfy the predicate (with the
#'   annotation columns when `annotate`).
#' @seealso [board_only_edges()], [non_board_edges()]
#' @examples
#' \dontrun{
#' xo <- link_cross_org(linked)
#' # strictly-paid interlocks (officer/staff only, no board, no volunteer OTHER):
#' filter_edges_by_role(xo$edges, xo$profiles,
#'                      target = c("OFFICER","STAFF"), mode = "all")
#' }
#' @export
filter_edges_by_role <- function(edges, profiles, target = "BOARD",
                                 mode = c("any", "all", "primary"),
                                 match = c("both", "either"),
                                 negate = FALSE, annotate = TRUE) {
  mode <- match.arg(mode); match <- match.arg(match)
  target <- toupper(target)
  if (!nrow(edges)) return(edges)

  ix <- stats::setNames(seq_len(nrow(profiles)), profiles$EMP_ID)
  role_sets <- profiles$roles
  primary <- as.character(profiles$primary_role)

  endpoint_ok <- function(idx) {
    vapply(idx, function(i) {
      if (is.na(i)) { val <- FALSE } else {
        rs <- role_sets[[i]]
        val <- switch(mode,
          any     = length(intersect(rs, target)) > 0L,
          all     = length(rs) > 0L && all(rs %in% target),
          primary = { p <- primary[i]; !is.na(p) && p %in% target })
      }
      if (negate) !val else val
    }, logical(1))
  }

  ia <- ix[edges$emp_a]; ib <- ix[edges$emp_b]
  qa <- endpoint_ok(ia); qb <- endpoint_ok(ib)
  keep <- if (match == "both") qa & qb else qa | qb

  out <- edges[keep, , drop = FALSE]
  if (annotate && nrow(out)) {
    collapse <- function(idx) vapply(idx, function(i)
      if (is.na(i) || !length(role_sets[[i]])) NA_character_
      else paste(role_sets[[i]], collapse = "+"), character(1))
    out$roles_a <- collapse(ia[keep]); out$roles_b <- collapse(ib[keep])
  }
  rownames(out) <- NULL
  out
}

#' Board-governance interlock edges
#'
#' Convenience wrapper for [filter_edges_by_role()] keeping edges where both
#' endpoints held a board seat -- interlocking-directorate ties. `mode = "any"`
#' (the default) counts a person who *ever* sat on that org's board; use
#' `mode = "primary"` to require board to be their dominant role there.
#'
#' @inheritParams filter_edges_by_role
#' @return The board-interlock subset of `edges`.
#' @examples
#' \dontrun{ board_only_edges(link_cross_org(linked)$edges, profiles) }
#' @export
board_only_edges <- function(edges, profiles, mode = c("any", "all", "primary"),
                             match = c("both", "either"), annotate = TRUE) {
  filter_edges_by_role(edges, profiles, target = "BOARD",
                       mode = match.arg(mode), match = match.arg(match),
                       negate = FALSE, annotate = annotate)
}

#' Non-board (paid-employee) edges
#'
#' Convenience wrapper for [filter_edges_by_role()] keeping edges where neither
#' endpoint held a board role -- the population for questions about *employment*
#' rather than governance (e.g. do paid staff get promoted faster by moving
#' organizations or by staying?). With the default `mode = "any"` an endpoint
#' qualifies only if it *never* held a board seat at that org (strict paid-only);
#' `mode = "primary"` relaxes this to "board was not their dominant role".
#'
#' Note this keeps `OFFICER`, `STAFF`, **and** `OTHER` (non-board volunteer roles
#' such as `CHAPLAIN`). For strictly *paid* employees, filter positively instead:
#' `filter_edges_by_role(edges, profiles, target = c("OFFICER","STAFF"),
#' mode = "all")`.
#'
#' @inheritParams filter_edges_by_role
#' @return The non-board subset of `edges`.
#' @examples
#' \dontrun{ non_board_edges(link_cross_org(linked)$edges, profiles) }
#' @export
non_board_edges <- function(edges, profiles, mode = c("any", "all", "primary"),
                            match = c("both", "either"), annotate = TRUE) {
  filter_edges_by_role(edges, profiles, target = "BOARD",
                       mode = match.arg(mode), match = match.arg(match),
                       negate = TRUE, annotate = annotate)
}
