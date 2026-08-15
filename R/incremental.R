#' Same-organization, surname-blocked bipartite candidate pairs
#'
#' Stage-1 blocking for incremental wave matching. Unlike the cross-org blocker
#' ([candidate_pairs()]), which crosses organizations and drops same-org pairs,
#' this keeps only pairs that share an organization -- a wave attaches *within* an
#' org -- and is bipartite: every pair has one `existing` person and one `wave`
#' person (never existing-existing or wave-wave). Two people share a bucket when
#' they share an organization and a surname phonetic key (`last_name_keys`), so
#' maiden/married/OCR surname variants still block.
#'
#' @param existing,wave Profiles ([build_person_profile()]) for the frozen panel
#'   and the new wave. Each within-org `EMP_ID` is one person in one org (its
#'   `orgs` list-column is a singleton).
#' @param max_block_size Skip (and report) any `(org, surname-key)` bucket whose
#'   existing x wave product exceeds this, to avoid a common-surname blow-up.
#' @return A data frame of unordered candidate pairs `emp_a` (existing), `emp_b`
#'   (wave). Oversize buckets skipped are in the `"dropped_blocks"` attribute.
#' @keywords internal
same_org_candidate_pairs <- function(existing, wave, max_block_size = 5000L) {
  org1 <- function(p) vapply(p$orgs, function(o) if (length(o)) o[[1]] else NA_character_,
                             character(1))
  ## Long (emp, bucket) tables: bucket = "<org>|<surname phonetic key>".
  explode <- function(p) {
    org <- org1(p); keys <- p$last_name_keys
    rows <- lapply(seq_len(nrow(p)), function(i) {
      k <- keys[[i]]; k <- k[!is.na(k) & nzchar(k)]
      if (!length(k) || is.na(org[i])) return(NULL)
      data.frame(emp = p$EMP_ID[i], bucket = paste(org[i], k, sep = "|"),
                 stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
  }
  el <- explode(existing); wl <- explode(wave)
  empty <- data.frame(emp_a = character(0), emp_b = character(0),
                      stringsAsFactors = FALSE)
  if (is.null(el) || is.null(wl)) { attr(empty, "dropped_blocks") <- NULL; return(empty) }

  be <- split(el$emp, el$bucket); bw <- split(wl$emp, wl$bucket)
  common <- intersect(names(be), names(bw))
  pa <- character(0); pb <- character(0); dropped <- list()
  for (bk in common) {
    ea <- unique(be[[bk]]); wb <- unique(bw[[bk]])
    if (length(ea) * length(wb) > max_block_size) {
      dropped[[length(dropped) + 1L]] <-
        data.frame(bucket = bk, n_existing = length(ea), n_wave = length(wb),
                   stringsAsFactors = FALSE)
      next
    }
    g <- expand.grid(emp_a = ea, emp_b = wb, stringsAsFactors = FALSE,
                     KEEP.OUT.ATTRS = FALSE)
    pa <- c(pa, g$emp_a); pb <- c(pb, g$emp_b)
  }
  res <- unique(data.frame(emp_a = pa, emp_b = pb, stringsAsFactors = FALSE))
  rownames(res) <- NULL
  attr(res, "dropped_blocks") <- if (length(dropped)) do.call(rbind, dropped) else NULL
  res
}

#' Match a new wave of people to an existing frozen person set
#'
#' The asymmetric core of incremental linkage (see `dev/PLAN-incremental-waves.md`).
#' Given profiles for an already-linked panel (`existing`, carrying durable
#' `EMP_ID`/`EMP_ANCHOR`) and profiles for a newly linked wave (`wave`, with its
#' own provisional wave-local `EMP_ID`s from a fresh [link_panel()] over the wave
#' alone), decide which wave-people are returning existing people and which are
#' new. It does **not** re-link existing-vs-existing and never renames an existing
#' id; it only maps wave ids onto existing ones (or leaves them to be minted
#' fresh).
#'
#' Pipeline: same-org surname blocking ([same_org_candidate_pairs()]) -> score the
#' candidate pairs from their variant sets ([score_candidate_pairs()]) -> drop
#' pairs that violate the one-record-per-(org,year) invariant (a wave-person and an
#' existing person who both file in the same org *and* year cannot be the same
#' person) -> accept the rest greedily one-to-one ([greedy_one_to_one()]).
#'
#' Surname evidence is weighted by *population* rarity ([population_surname_weight()])
#' over the combined profile set, not within-org frequency: at the match stage both
#' people are already distinct identities, so the question is how surprising the
#' surname agreement is across the wave boundary (`SMITH` weak, `GANTSOUDES`
#' strong) -- the same logic as cross-org linkage. Within-org family-board
#' separation was already handled when the wave was linked internally.
#'
#' @param wave Profiles of the internally linked new wave ([build_person_profile()]
#'   over `link_panel(new)`).
#' @param existing Profiles of the frozen panel; should carry `EMP_ANCHOR` (via
#'   [build_person_profile()] on an anchored [link_panel()] output) so matches can
#'   inherit it.
#' @param weights Base match weights; see [default_weights()].
#' @param threshold Minimum score to accept a wave<->existing match (default `7`,
#'   the within-org default; **profile-vs-profile scores want their own tuning**).
#' @param surname_weight Optional precomputed surname-rarity vector; the population
#'   weight over `rbind(existing, wave)` if `NULL`.
#' @param max_block_size Oversize-bucket guard for [same_org_candidate_pairs()].
#' @param verbose Print stage sizes.
#' @return A list:
#'   \describe{
#'     \item{`matched`}{`wave_emp_id`, `existing_emp_id`, `existing_emp_anchor`
#'       (if available), `score` -- one row per accepted match.}
#'     \item{`unmatched`}{Character vector of wave `EMP_ID`s with no accepted
#'       match (the first-time people, to be minted fresh downstream).}
#'     \item{`review`}{Above-threshold pairs that were *not* accepted, with a
#'       `reason`: `"ambiguous"` (lost the greedy one-to-one -- a second plausible
#'       match worth a look) or `"invariant_collision"` (strong score but the two
#'       share an org-year, so they cannot be one person).}
#'     \item{`report`}{Counts for the run.}
#'   }
#' @seealso [same_org_candidate_pairs()], [score_candidate_pairs()],
#'   [population_surname_weight()]
#' @export
match_to_profiles <- function(wave, existing, weights = default_weights(),
                              threshold = 7, surname_weight = NULL,
                              max_block_size = 5000L, verbose = FALSE) {
  say <- function(...) if (verbose) message(...)
  stopifnot(is.data.frame(wave), is.data.frame(existing))
  if (!"EMP_ID" %in% names(wave) || !"EMP_ID" %in% names(existing)) {
    stop("match_to_profiles(): both `wave` and `existing` need an EMP_ID column.",
         call. = FALSE)
  }
  clash <- intersect(existing$EMP_ID, wave$EMP_ID)
  if (length(clash)) {
    stop("match_to_profiles(): wave and existing EMP_IDs overlap (", length(clash),
         "); wave ids must come from a separate link_panel() run over the wave alone.",
         call. = FALSE)
  }

  empty_matched <- data.frame(wave_emp_id = character(0), existing_emp_id = character(0),
                              existing_emp_anchor = character(0), score = numeric(0),
                              stringsAsFactors = FALSE)
  empty_review <- data.frame(wave_emp_id = character(0), existing_emp_id = character(0),
                             score = numeric(0), reason = character(0),
                             stringsAsFactors = FALSE)
  finish <- function(matched, review, n_cand, n_coll) {
    list(matched = matched, review = review,
         unmatched = setdiff(wave$EMP_ID, matched$wave_emp_id),
         report = list(n_wave_persons = nrow(wave), n_existing_persons = nrow(existing),
                       n_candidate_pairs = n_cand, n_matched = nrow(matched),
                       n_new_persons = length(setdiff(wave$EMP_ID, matched$wave_emp_id)),
                       n_invariant_collisions = n_coll,
                       n_ambiguous = sum(review$reason == "ambiguous")))
  }

  say("Blocking wave x existing within org...")
  cand <- same_org_candidate_pairs(existing, wave, max_block_size = max_block_size)
  say("  ", nrow(cand), " candidate pairs")
  if (!nrow(cand)) return(finish(empty_matched, empty_review, 0L, 0L))

  ## Combined profile table (disjoint ids) for the shared scorer + year lookup.
  common_cols <- intersect(names(existing), names(wave))
  combined <- rbind(existing[, common_cols, drop = FALSE],
                    wave[, common_cols, drop = FALSE])
  if (is.null(surname_weight)) surname_weight <- population_surname_weight(combined)

  say("Scoring candidate pairs...")
  scored <- score_candidate_pairs(cand, combined, weights = weights,
                                   surname_weight = surname_weight)
  above <- scored[!is.na(scored$score) & scored$score >= threshold, , drop = FALSE]
  if (!nrow(above)) return(finish(empty_matched, empty_review, nrow(cand), 0L))

  ## One-record-per-(org,year) invariant. Same org by construction (blocked on
  ## org), so a collision is a non-empty overlap of the two people's year sets.
  years_by <- stats::setNames(combined$years, combined$EMP_ID)
  collide <- vapply(seq_len(nrow(above)), function(k) {
    ya <- years_by[[above$emp_a[k]]]; yb <- years_by[[above$emp_b[k]]]
    length(intersect(ya, yb)) > 0L
  }, logical(1))
  legal <- above[!collide, , drop = FALSE]
  collided <- above[collide, , drop = FALSE]

  say("Accepting greedy one-to-one (", sum(!collide), " legal, ",
      sum(collide), " invariant collisions)...")
  acc <- if (nrow(legal)) {
    greedy_one_to_one(data.frame(row_x = legal$emp_a, row_y = legal$emp_b,
                                 score = legal$score, stringsAsFactors = FALSE))
  } else data.frame(row_x = character(0), row_y = character(0), score = numeric(0))

  anchor_by <- if ("EMP_ANCHOR" %in% names(existing))
    stats::setNames(as.character(existing$EMP_ANCHOR), existing$EMP_ID) else NULL
  matched <- data.frame(
    wave_emp_id = acc$row_y, existing_emp_id = acc$row_x,
    existing_emp_anchor = if (is.null(anchor_by)) rep(NA_character_, nrow(acc))
                          else unname(anchor_by[acc$row_x]),
    score = acc$score, stringsAsFactors = FALSE
  )

  ## Review: above-threshold, legal pairs that lost the greedy pick (ambiguous),
  ## plus the invariant collisions (structurally impossible but strong-scoring).
  accepted_key <- paste(matched$existing_emp_id, matched$wave_emp_id, sep = "\r")
  legal_key <- paste(legal$emp_a, legal$emp_b, sep = "\r")
  amb <- legal[!legal_key %in% accepted_key, , drop = FALSE]
  review <- rbind(
    data.frame(wave_emp_id = amb$emp_b, existing_emp_id = amb$emp_a,
               score = amb$score, reason = rep("ambiguous", nrow(amb)),
               stringsAsFactors = FALSE),
    data.frame(wave_emp_id = collided$emp_b, existing_emp_id = collided$emp_a,
               score = collided$score,
               reason = rep("invariant_collision", nrow(collided)),
               stringsAsFactors = FALSE)
  )
  review <- review[order(-review$score), , drop = FALSE]
  rownames(review) <- NULL

  finish(matched, review, nrow(cand), nrow(collided))
}
