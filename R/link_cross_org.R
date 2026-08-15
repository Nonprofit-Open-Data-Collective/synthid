#' Population surname-rarity weight
#'
#' The cross-organization analogue of the within-organization [surname_weight()].
#' Within an organization the question is "how many *distinct people here* share
#' this surname" (a family board); across organizations the question is "how rare
#' is this surname in the *population*" -- two people named `SMITH` agreeing across
#' orgs is weak evidence, two named `GANTSOUDES` is strong. Returns a normalized
#' inverse-document-frequency weight per surname: `log(N / n_s) / log(N)`, where
#' `N` is the number of people and `n_s` the number whose canonical surname is
#' `s`. A unique surname scores `~1`; a surname everyone shares scores `~0`.
#'
#' @param profiles Output of [build_person_profile()] (uses the canonical
#'   `last_name`, one per person).
#' @return A named numeric vector (surname -> weight in `[0, 1]`).
#' @export
population_surname_weight <- function(profiles) {
  s <- toupper(trimws(as.character(profiles$last_name)))
  s[is.na(s) | !nzchar(s)] <- NA
  N <- sum(!is.na(s))
  if (N <= 1L) {
    u <- unique(s[!is.na(s)])
    return(stats::setNames(rep(1, length(u)), u))
  }
  tb <- table(s)
  w <- log(N / as.numeric(tb)) / log(N)
  stats::setNames(pmin(pmax(w, 0), 1), names(tb))
}

## Best (max) similarity over the cross product of two variant sets, using a
## vectorized pairwise comparator cmp(vec1, vec2). NA if either set is empty.
.best_set_sim <- function(a, b, cmp) {
  if (!length(a) || !length(b)) return(NA_real_)
  g <- expand.grid(a = a, b = b, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  v <- cmp(g$a, g$b)
  if (all(is.na(v))) return(NA_real_)
  max(v, na.rm = TRUE)
}

## Set-overlap comparator: 1 if the sets share a value, 0 if both non-empty and
## disjoint, NA if either is empty (missing -> neutral in score_pairs()).
.set_overlap <- function(a, b) {
  if (!length(a) || !length(b)) return(NA_real_)
  as.numeric(length(intersect(a, b)) > 0L)
}

#' Score cross-organization candidate pairs from their profiles
#'
#' Stage 2 of cross-org linkage: turns each candidate pair from [candidate_pairs()]
#' into a Fellegi-Sunter-style additive match score, reusing the within-org
#' weighting ([default_weights()], [score_pairs()]) but computing every field
#' similarity from the person *variant sets* rather than a single string:
#' \itemize{
#'   \item `last_name` -- best match over `last_name_variants_a x _b`
#'     ([compare_two_names()]), frequency-adjusted by the *population* surname
#'     rarity ([population_surname_weight()]);
#'   \item `first_name` -- best match over `first_name_variants` with nickname and
#'     initial handling ([compare_first_names()]);
#'   \item `middle_name`, `suffix` -- set overlap of the initials / suffixes
#'     (agree / disagree / neutral-if-missing);
#'   \item `gender` -- exact agreement of the modal genders.
#' }
#' Title is deliberately excluded -- per the interlock design it is a downstream
#' edge filter, not a linkage signal.
#'
#' @param cand Candidate pairs from [candidate_pairs()] (`emp_a`, `emp_b`, ...).
#' @param profiles The profiles those ids index (from [build_person_profile()]).
#' @param weights Named base weights; see [default_weights()].
#' @param surname_weight Optional precomputed [population_surname_weight()] vector;
#'   built from `profiles` if `NULL`.
#' @param components If `TRUE`, also return the per-field similarity columns
#'   (`sim_last`, `sim_first`, ...) for inspection/tuning.
#' @return `cand` with an added numeric `score` column (and, if `components`, the
#'   per-field similarities), sorted by descending score.
#' @seealso [candidate_pairs()], [link_cross_org()]
#' @export
score_candidate_pairs <- function(cand, profiles, weights = default_weights(),
                                   surname_weight = NULL, components = FALSE) {
  if (!nrow(cand)) {
    cand$score <- numeric(0)
    return(cand)
  }
  if (is.null(surname_weight)) surname_weight <- population_surname_weight(profiles)

  ## O(1) lookups by EMP_ID into the variant sets and scalars.
  ix <- stats::setNames(seq_len(nrow(profiles)), profiles$EMP_ID)
  ln_var <- profiles$last_name_variants
  fn_var <- profiles$first_name_variants
  mid    <- profiles$middle_initials
  suf    <- profiles$suffixes
  ln_can <- toupper(trimws(as.character(profiles$last_name)))
  gender <- as.character(profiles$gender)

  ia <- ix[cand$emp_a]; ib <- ix[cand$emp_b]
  n <- nrow(cand)
  sim_last <- sim_first <- sim_mid <- sim_suf <- sim_gen <- numeric(n)

  for (k in seq_len(n)) {
    a <- ia[k]; b <- ib[k]
    sim_last[k]  <- .best_set_sim(ln_var[[a]], ln_var[[b]], compare_last_names)
    sim_first[k] <- .best_set_sim(fn_var[[a]], fn_var[[b]], compare_first_names)
    sim_mid[k]   <- .set_overlap(mid[[a]], mid[[b]])
    sim_suf[k]   <- .set_overlap(suf[[a]], suf[[b]])
    ga <- gender[a]; gb <- gender[b]
    sim_gen[k]   <- if (is.na(ga) || is.na(gb)) NA_real_ else as.numeric(ga == gb)
  }

  cmp <- data.frame(
    last_name   = sim_last,
    first_name  = sim_first,
    middle_name = sim_mid,
    suffix      = sim_suf,
    gender      = sim_gen,
    stringsAsFactors = FALSE
  )
  sw_a <- unname(surname_weight[ln_can[ia]]); sw_a[is.na(sw_a)] <- 1
  sw_b <- unname(surname_weight[ln_can[ib]]); sw_b[is.na(sw_b)] <- 1

  cand$score <- score_pairs(cmp, weights, sw_a, sw_b, last_field = "last_name")
  if (components) {
    names(cmp) <- paste0("sim_", names(cmp))
    cand <- cbind(cand, cmp)
  }
  cand[order(-cand$score), , drop = FALSE]
}

#' Resolve scored cross-org edges into cross-organization person clusters
#'
#' Closes the accepted cross-org edges into connected components, enforcing the
#' invariant that no cross-org person may contain two `EMP_ID`s from the *same*
#' organization -- two distinct within-org identities in one organization are two
#' different people by construction, so a transitive chain that would merge them
#' is rejected (highest-score edges kept first), exactly mirroring the year guard
#' in the within-org resolver.
#'
#' @param edges Accepted edges (`emp_a`, `emp_b`, `score`).
#' @param profiles Profiles supplying each person's organization.
#' @return A data frame `EMP_ID`, `XORG_ID` (deterministic cross-org person id,
#'   anchored to the interlock's founding member -- earliest `first_year` -- so it
#'   survives a refresh adding members ([anchor_xorg_id()]); singletons get their
#'   own), `XORG_N_ORGS`, `XORG_N_TITLES` (distinct titles),
#'   `XORG_N_YEARS` (distinct calendar years), and `XORG_N_RECORDS` (total filing
#'   rows) across the whole cross-org person. `rejected_edges` count is attached
#'   as an attribute.
#' @keywords internal
resolve_cross_org <- function(edges, profiles) {
  emp <- profiles$EMP_ID
  org1 <- vapply(profiles$orgs, function(o) if (length(o)) o[[1]] else NA_character_,
                 character(1))
  idx <- seq_along(emp); names(idx) <- emp
  parent <- idx
  orgs_in_set <- as.list(org1)

  find <- function(a) {
    root <- a
    while (parent[root] != root) root <- parent[root]
    while (parent[a] != root) { nxt <- parent[a]; parent[a] <<- root; a <- nxt }
    root
  }

  rejected <- 0L
  if (!is.null(edges) && nrow(edges) > 0L) {
    ord <- order(-edges$score)
    ea <- idx[edges$emp_a]; eb <- idx[edges$emp_b]
    for (k in ord) {
      ra <- find(ea[k]); rb <- find(eb[k])
      if (is.na(ra) || is.na(rb) || ra == rb) next
      oa <- orgs_in_set[[ra]]; ob <- orgs_in_set[[rb]]
      if (length(intersect(oa, ob)) > 0L) { rejected <- rejected + 1L; next }
      if (length(oa) < length(ob)) { tmp <- ra; ra <- rb; rb <- tmp }
      parent[rb] <- ra
      orgs_in_set[[ra]] <- c(orgs_in_set[[ra]], orgs_in_set[[rb]])
    }
  }

  roots <- vapply(idx, find, integer(1))
  members <- split(emp, roots)
  ## Anchor each interlock to its founding member -- the EMP_ID with the earliest
  ## first_year (ties broken by EMP_ID) -- so the XORG_ID survives a person being
  ## added on a later cross-org refresh (see anchor_xorg_id()).
  first_year_by <- stats::setNames(suppressWarnings(as.integer(profiles$first_year)), emp)
  xid <- vapply(members, function(m) {
    fy <- first_year_by[m]
    anchor <- m[order(fy, m, na.last = TRUE)][1]
    anchor_xorg_id(anchor)
  }, character(1))
  norg <- vapply(members, function(m) length(unique(org1[match(m, emp)])), integer(1))
  ## Per-cluster rollups over the member EMP_IDs: distinct titles and distinct
  ## calendar years are UNIONS of the per-tenure sets (a person on two boards in
  ## one year is one year); records are a SUM (total filing rows).
  titles_list <- if ("titles" %in% names(profiles)) profiles$titles else NULL
  years_list  <- if ("years"  %in% names(profiles)) profiles$years  else NULL
  nrec_v      <- if ("n_records" %in% names(profiles)) profiles$n_records else NULL
  ntitle <- vapply(members, function(m) {
    if (is.null(titles_list)) return(NA_integer_)
    length(unique(unlist(titles_list[match(m, emp)], use.names = FALSE)))
  }, integer(1))
  nyear <- vapply(members, function(m) {
    if (is.null(years_list)) return(NA_integer_)
    length(unique(unlist(years_list[match(m, emp)], use.names = FALSE)))
  }, integer(1))
  nrec <- vapply(members, function(m) {
    if (is.null(nrec_v)) return(NA_integer_)
    as.integer(sum(nrec_v[match(m, emp)], na.rm = TRUE))
  }, integer(1))

  map <- data.frame(
    EMP_ID = emp,
    XORG_ID = xid[as.character(roots)],
    XORG_N_ORGS = norg[as.character(roots)],
    XORG_N_TITLES = ntitle[as.character(roots)],
    XORG_N_YEARS = nyear[as.character(roots)],
    XORG_N_RECORDS = nrec[as.character(roots)],
    stringsAsFactors = FALSE
  )
  attr(map, "rejected_edges") <- rejected
  map
}

#' Cross-organization person linkage (interlock) pipeline
#'
#' End-to-end stage-1 + stage-2 cross-org linkage over a panel that already
#' carries within-org `EMP_ID`s ([link_panel()]): build one profile per person,
#' generate candidate pairs by hash blocking, score them from the variant sets,
#' link pairs at or above `threshold`, and resolve the accepted edges into
#' cross-organization person clusters (`XORG_ID`) under the one-person-per-org
#' invariant.
#'
#' @param df A linked panel (output of [link_panel()]).
#' @param cols Column mapping; see [synthid_cols()].
#' @param state,ntee Optional organization geography / NTEE columns for the tighter
#'   blocking passes (join from the BMF beforehand). `NULL` -> those passes emit
#'   nothing and linkage falls back to the surname pass.
#' @param passes Blocking passes to run; see [blocking_passes()].
#' @param weights Base match weights; see [default_weights()].
#' @param threshold Minimum score to accept a cross-org edge (default `7`, the
#'   within-org default; **needs its own tuning** -- the score scale differs, as
#'   title/salutation are dropped and surname rarity is population-based).
#' @param max_block_size Oversize-bucket guard for [candidate_pairs()].
#' @param verbose Print stage sizes.
#' @return A list: `profiles` and `assignment` both carry the cross-org rollups
#'   `XORG_ID`, `XORG_N_ORGS`, `XORG_N_TITLES`, `XORG_N_YEARS` (distinct calendar
#'   years), `XORG_N_RECORDS` (total filing rows); `edges` (all scored candidate
#'   pairs); and `report` (counts, including edges rejected by the org guard).
#'   Stamp the assignment back onto the panel with [stamp_xorg()] to recover each
#'   person's full title history.
#' @examples
#' \dontrun{
#' linked <- link_panel(panel)
#' xo <- link_cross_org(linked, state = "state", ntee = "ntee", threshold = 8)
#' subset(xo$profiles, XORG_N_ORGS > 1)          # people who interlock
#' subset(xo$edges, score >= 8 & is_board_a & is_board_b)  # board-board interlocks
#' }
#' @seealso [build_person_profile()], [person_blocking_keys()],
#'   [candidate_pairs()], [score_candidate_pairs()]
#' @export
link_cross_org <- function(df, cols = synthid_cols(),
                           state = NULL, ntee = NULL,
                           passes = names(blocking_passes()),
                           weights = default_weights(),
                           threshold = 7, max_block_size = 5000L,
                           verbose = FALSE) {
  say <- function(...) if (verbose) message(...)

  say("Building person profiles...")
  profiles <- build_person_profile(df, cols = cols, state = state, ntee = ntee)
  say("  ", nrow(profiles), " persons")

  say("Emitting blocking keys...")
  keys <- person_blocking_keys(profiles, passes = passes)

  say("Hash-joining candidate pairs...")
  cand <- candidate_pairs(keys, profiles, max_block_size = max_block_size)
  say("  ", nrow(cand), " candidate pairs (naive all-pairs: ",
      format(choose(nrow(profiles), 2), big.mark = ","), ")")

  say("Scoring candidates...")
  scored <- score_candidate_pairs(cand, profiles, weights = weights)
  edges <- scored[!is.na(scored$score) & scored$score >= threshold, , drop = FALSE]
  say("  ", nrow(edges), " edges at threshold ", threshold)

  say("Resolving cross-org clusters...")
  map <- resolve_cross_org(edges, profiles)
  mi <- match(profiles$EMP_ID, map$EMP_ID)
  profiles$XORG_ID <- map$XORG_ID[mi]
  profiles$XORG_N_ORGS <- map$XORG_N_ORGS[mi]
  profiles$XORG_N_TITLES <- map$XORG_N_TITLES[mi]
  profiles$XORG_N_YEARS <- map$XORG_N_YEARS[mi]
  profiles$XORG_N_RECORDS <- map$XORG_N_RECORDS[mi]

  report <- list(
    n_persons = nrow(profiles),
    n_candidate_pairs = nrow(scored),
    n_edges = nrow(edges),
    n_rejected_org_collision = attr(map, "rejected_edges"),
    n_interlocking_persons = sum(map$XORG_N_ORGS > 1L),
    n_cross_org_clusters = length(unique(map$XORG_ID[map$XORG_N_ORGS > 1L]))
  )
  say("  ", report$n_interlocking_persons, " persons span >1 org")

  list(profiles = profiles, edges = scored, assignment = map, report = report)
}

#' Stamp cross-org person ids back onto the record-level panel
#'
#' Joins the cross-org `assignment` ([link_cross_org()]) onto the original
#' record-level panel by `EMP_ID`, so every filing row of a real person -- across
#' every organization and tax year -- carries one `XORG_ID`. Grouping the stamped
#' panel by `XORG_ID` then recovers a person's complete title/year history across
#' organizations (grouping by `EMP_ID` gives it within a single organization).
#'
#' @param df The record-level panel that was linked (carries `EMP_ID`).
#' @param x Either the `assignment` data frame from [link_cross_org()] or the full
#'   `link_cross_org()` result list (its `$assignment` is used).
#' @param emp_id Name of the person-cluster id column (default `"EMP_ID"`).
#' @param cols Cross-org columns to copy over; defaults to all present in the
#'   assignment (`XORG_ID`, `XORG_N_ORGS`, `XORG_N_TITLES`, `XORG_N_YEARS`,
#'   `XORG_N_RECORDS`).
#' @return `df` with the requested cross-org columns added. Rows whose `EMP_ID` is
#'   absent from the assignment get `NA` (and a warning).
#' @examples
#' \dontrun{
#' xo <- link_cross_org(linked)
#' panel <- stamp_xorg(linked, xo)
#' # a person's full cross-org title history:
#' subset(panel, XORG_ID == panel$XORG_ID[1])[, c("ein","taxyr","title.standard")]
#' }
#' @seealso [link_cross_org()]
#' @export
stamp_xorg <- function(df, x, emp_id = "EMP_ID",
                       cols = c("XORG_ID", "XORG_N_ORGS", "XORG_N_TITLES",
                                "XORG_N_YEARS", "XORG_N_RECORDS")) {
  assignment <- if (is.data.frame(x)) x
                else if (is.list(x) && !is.null(x$assignment)) x$assignment
                else stop("stamp_xorg(): `x` must be an assignment data frame or a ",
                          "link_cross_org() result list.", call. = FALSE)
  if (!emp_id %in% names(df)) {
    stop("stamp_xorg(): '", emp_id, "' not found in `df`.", call. = FALSE)
  }
  cols <- intersect(cols, names(assignment))
  m <- match(as.character(df[[emp_id]]), as.character(assignment$EMP_ID))
  if (anyNA(m)) {
    warning(sum(is.na(m)), " row(s) have an EMP_ID absent from the assignment; ",
            "cross-org columns set to NA there.", call. = FALSE)
  }
  for (cc in cols) df[[cc]] <- assignment[[cc]][m]
  df
}
