#' Blocking passes for cross-organization candidate generation
#'
#' The relaxation ladder, tightest first. Each pass names the profile components
#' that must agree for two people to share a bucket. Geography is relaxed before
#' industry (people move; cross-industry interlocks are the interesting edges) and
#' surname is never fully dropped.
#'
#' @return A named list; each element is the character vector of key components
#'   for that pass. Order is precedence (element 1 = tightest).
#' @export
blocking_passes <- function() {
  list(
    strict   = c("last", "state", "ntee"), # surname x geography x industry
    geo      = c("last", "ntee"),          # geography relaxed
    industry = c("last", "state"),         # industry relaxed
    surname  = c("last")                   # both relaxed -- surname only
  )
}

#' Explode person profiles into a long blocking-key table
#'
#' Turns each [build_person_profile()] row into the set of hash buckets it belongs
#' to -- the stage-1 input for cross-organization linkage. Because a profile keeps
#' its *variant sets*, it emits the Cartesian product of its surname phonetic keys,
#' states, and (truncated) NTEE codes for each requested pass, so a person with a
#' maiden and a married surname, or activity in two states, lands in every
#' matching bucket instead of a single one.
#'
#' Keys are namespaced by pass, so a `strict` bucket and a `surname` bucket never
#' collide even when they share a surname key. A pass that needs a component the
#' profile lacks (e.g. `strict` when `states` is empty) simply emits nothing for
#' that profile.
#'
#' @param profiles Output of [build_person_profile()] (needs `last_name_keys`;
#'   `states`/`ntee` list-columns for the geography/industry passes).
#' @param passes Character vector naming the passes to emit (see
#'   [blocking_passes()]); default all four.
#' @param ntee_digits Number of leading characters of each NTEE code to key on
#'   (default `1` = major group, e.g. `"B25"` -> `"B"`).
#' @return A data frame `EMP_ID`, `pass`, `block_key` (one row per bucket a person
#'   lands in). `block_key` is `"<pass>|<last>[|<state>][|<ntee>]"`.
#' @examples
#' \dontrun{
#' profiles <- build_person_profile(linked, state = "state", ntee = "ntee")
#' keys <- person_blocking_keys(profiles)
#' table(keys$pass)
#' }
#' @seealso [candidate_pairs()] for the hash join over these keys.
#' @export
person_blocking_keys <- function(profiles,
                                 passes = names(blocking_passes()),
                                 ntee_digits = 1L) {
  spec <- blocking_passes()
  bad <- setdiff(passes, names(spec))
  if (length(bad)) {
    stop("person_blocking_keys(): unknown pass(es): ", paste(bad, collapse = ", "),
         call. = FALSE)
  }
  has_state <- "states" %in% names(profiles)
  has_ntee  <- "ntee"   %in% names(profiles)

  getset <- function(col, i, trunc = NULL) {
    if (is.null(col)) return(character(0))
    v <- col[[i]]
    v <- v[!is.na(v) & nzchar(v)]
    if (!is.null(trunc) && length(v)) v <- unique(substr(v, 1, trunc))
    v
  }

  out <- vector("list", nrow(profiles))
  for (i in seq_len(nrow(profiles))) {
    last  <- getset(profiles$last_name_keys, i)
    if (!length(last)) next
    state <- if (has_state) getset(profiles$states, i) else character(0)
    ntee  <- if (has_ntee)  getset(profiles$ntee,  i, trunc = ntee_digits) else character(0)

    rows <- list()
    for (pn in passes) {
      comps <- spec[[pn]]
      ## Build the component grids this pass requires; empty grid -> no keys.
      grids <- list(last = last)
      if ("state" %in% comps) grids$state <- state
      if ("ntee"  %in% comps) grids$ntee  <- ntee
      if (any(lengths(grids) == 0L)) next
      g <- expand.grid(grids[comps], stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
      key <- do.call(paste, c(list(pn), g[comps], sep = "|"))
      rows[[pn]] <- data.frame(EMP_ID = profiles$EMP_ID[i], pass = pn,
                               block_key = key, stringsAsFactors = FALSE)
    }
    if (length(rows)) out[[i]] <- do.call(rbind, rows)
  }
  res <- do.call(rbind, out)
  if (is.null(res)) {
    return(data.frame(EMP_ID = character(0), pass = character(0),
                      block_key = character(0), stringsAsFactors = FALSE))
  }
  res <- unique(res)
  rownames(res) <- NULL
  res
}

#' Generate cross-organization candidate pairs by hash join
#'
#' Stage 1 of cross-org linkage: instead of a materialized within-block cross
#' product (the reclin2 `pair_blocking` approach used within an organization),
#' this groups profiles by hash bucket ([person_blocking_keys()]) and emits the
#' distinct person pairs that co-occur in a bucket. Only these candidates are
#' handed to the (expensive) stage-2 scorer.
#'
#' Since each within-organization `EMP_ID` denotes one person in one organization,
#' a genuine interlock links two `EMP_ID`s in *different* organizations, so pairs
#' sharing an organization are dropped. Buckets larger than `max_block_size`
#' (typically a very common surname with geography/industry relaxed away) would
#' emit O(n^2) mostly-spurious pairs; they are skipped and reported in the
#' `"dropped_blocks"` attribute rather than silently truncated.
#'
#' @param keys Long key table from [person_blocking_keys()].
#' @param profiles The profiles the keys came from; supplies each person's
#'   organization (from `orgs`) for the same-organization filter.
#' @param max_block_size Skip (and report) any bucket with more than this many
#'   distinct people (default `5000`).
#' @return A data frame, one row per unordered candidate pair: `emp_a`, `emp_b`
#'   (string-sorted), `pass` and `pass_rank` of the *tightest* bucket that
#'   produced the pair, and `n_shared_buckets` (how many buckets they co-occur in
#'   -- a cheap first-pass strength signal). Attributes: `"dropped_blocks"` (over-
#'   size buckets skipped) and `"n_comparisons"` (pairs generated, i.e. the
#'   stage-2 workload).
#' @examples
#' \dontrun{
#' keys  <- person_blocking_keys(profiles)
#' cand  <- candidate_pairs(keys, profiles)
#' attr(cand, "n_comparisons")            # vs choose(nrow(profiles), 2)
#' }
#' @seealso [person_blocking_keys()], [compare_last_names()], [compare_first_names()]
#' @export
candidate_pairs <- function(keys, profiles, max_block_size = 5000L) {
  rank <- setNames(seq_along(blocking_passes()), names(blocking_passes()))

  ## One organization per within-org EMP_ID (orgs list-col holds a singleton).
  org1 <- vapply(profiles$orgs, function(o) if (length(o)) o[[1]] else NA_character_,
                 character(1))
  emp_org <- setNames(org1, profiles$EMP_ID)

  by_block <- split(keys$EMP_ID, keys$block_key)
  pass_of  <- keys$pass[!duplicated(keys$block_key)]
  names(pass_of) <- keys$block_key[!duplicated(keys$block_key)]

  pair_key <- character(0); pr <- integer(0); pp <- character(0)
  dropped <- list()
  for (bk in names(by_block)) {
    members <- unique(by_block[[bk]])
    if (length(members) < 2L) next
    if (length(members) > max_block_size) {
      dropped[[length(dropped) + 1L]] <-
        data.frame(block_key = bk, pass = pass_of[[bk]], n = length(members),
                   stringsAsFactors = FALSE)
      next
    }
    cb <- utils::combn(sort(members), 2L)
    a <- cb[1L, ]; b <- cb[2L, ]
    keep <- emp_org[a] != emp_org[b] & !is.na(emp_org[a]) & !is.na(emp_org[b])
    if (!any(keep)) next
    pair_key <- c(pair_key, paste(a[keep], b[keep], sep = "\r"))
    pp <- c(pp, rep(pass_of[[bk]], sum(keep)))
    pr <- c(pr, rep(rank[[pass_of[[bk]]]], sum(keep)))
  }

  if (!length(pair_key)) {
    res <- data.frame(emp_a = character(0), emp_b = character(0),
                      pass = character(0), pass_rank = integer(0),
                      n_shared_buckets = integer(0), stringsAsFactors = FALSE)
    attr(res, "dropped_blocks") <- do.call(rbind, dropped)
    attr(res, "n_comparisons") <- 0L
    return(res)
  }

  ## Collapse to one row per unordered pair: tightest pass, and bucket multiplicity.
  n_shared <- tapply(pair_key, pair_key, length)
  best_ord <- order(pair_key, pr)
  first    <- !duplicated(pair_key[best_ord])
  sel      <- best_ord[first]
  up       <- pair_key[sel]
  parts    <- do.call(rbind, strsplit(up, "\r", fixed = TRUE))

  res <- data.frame(
    emp_a = parts[, 1], emp_b = parts[, 2],
    pass = pp[sel], pass_rank = pr[sel],
    n_shared_buckets = as.integer(n_shared[up]),
    stringsAsFactors = FALSE
  )
  res <- res[order(res$pass_rank, -res$n_shared_buckets), ]
  rownames(res) <- NULL
  attr(res, "dropped_blocks") <- if (length(dropped)) do.call(rbind, dropped) else NULL
  attr(res, "n_comparisons") <- nrow(res)
  res
}
