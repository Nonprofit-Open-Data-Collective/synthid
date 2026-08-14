#' Resolve accepted pairwise links into stable person clusters
#'
#' Closes the accepted year-to-year edges into connected components while
#' enforcing the invariant that a cluster contains at most one record per
#' organization-year. Edges are added highest-score first; an edge that would
#' merge two records sharing an organization-year is rejected (and counted), so
#' the closure keeps the strongest evidence and never fuses two distinct people.
#' Every record -- including those in no accepted edge -- ends up in exactly one
#' cluster and receives one deterministic `EMP_ID`.
#'
#' All edges are within a single organization (candidate pairs are blocked on
#' organization), so the invariant reduces to "at most one record per year" within
#' a cluster.
#'
#' @param work Prepared working frame from [prepare_panel()] (needs `.row_uid`
#'   and `.year`).
#' @param edges Accepted edges from [link_year_pairs()].
#' @return A list with `assignment` (data frame `.row_uid`, `.emp_cluster`) and
#'   `rejected_edges` (count of edges dropped by the year-collision guard).
#' @keywords internal
resolve_clusters <- function(work, edges) {
  uids <- work$.row_uid
  n <- length(uids)
  idx <- seq_len(n)
  names(idx) <- uids
  year_of <- as.character(work$.year)

  ## Union-find with, per set, the set of years its members occupy.
  parent <- idx
  years_in_set <- as.list(year_of) # one year per singleton initially

  find <- function(a) {
    root <- a
    while (parent[root] != root) root <- parent[root]
    while (parent[a] != root) {
      nxt <- parent[a]
      parent[a] <<- root
      a <- nxt
    }
    root
  }

  rejected <- 0L
  if (nrow(edges) > 0L) {
    ord <- order(-edges$score)
    ex <- idx[edges$row_x]
    ey <- idx[edges$row_y]
    for (k in ord) {
      ra <- find(ex[k])
      rb <- find(ey[k])
      if (ra == rb) next
      ya <- years_in_set[[ra]]
      yb <- years_in_set[[rb]]
      if (length(intersect(ya, yb)) > 0L) {
        rejected <- rejected + 1L
        next
      }
      ## union: attach smaller tree under larger (by year count)
      if (length(ya) < length(yb)) {
        tmp <- ra; ra <- rb; rb <- tmp
      }
      parent[rb] <- ra
      years_in_set[[ra]] <- c(years_in_set[[ra]], years_in_set[[rb]])
    }
  }

  roots <- vapply(idx, find, integer(1))
  assignment <- data.frame(
    .row_uid = uids,
    .emp_cluster = roots,
    stringsAsFactors = FALSE
  )
  list(assignment = assignment, rejected_edges = rejected)
}

#' Assign a stable EMP_ID to each cluster
#'
#' The identifier is a deterministic hash of the sorted member record strings, so
#' it depends only on cluster membership (not on row order or run) and is stable
#' as long as the members are.
#'
#' @param assignment Data frame with `.row_uid` and `.emp_cluster`.
#' @return `assignment` with an added `EMP_ID` column.
#' @keywords internal
assign_emp_ids <- function(assignment) {
  split_uids <- split(assignment$.row_uid, assignment$.emp_cluster)
  canon <- vapply(split_uids, function(u) paste(sort(u), collapse = "||"),
                  character(1))
  emp <- create_emp_ids(canon)
  map <- data.frame(
    .emp_cluster = as.integer(names(split_uids)),
    EMP_ID = emp,
    stringsAsFactors = FALSE
  )
  merge(assignment, map, by = ".emp_cluster", sort = FALSE)
}
