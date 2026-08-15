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
#'   tuned on the 2019-2021 panel: post-greedy precision/recall are flat across
#'   5-8, so the greedy one-to-one -- not the cutoff -- carries the accuracy;
#'   see `dev/NOTES-match-threshold.md`. Raise toward 12 for near-1.0 precision at
#'   a steep recall cost; lower toward 5 to admit more OCR/nickname variants).
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

#' Integrate a new wave of data into an already-linked panel
#'
#' End-to-end incremental linkage: attach a new wave of records (a later year, a
#' multi-year batch, or a backfill of missing years) to a frozen, already-linked
#' panel, **reusing the existing person ids instead of re-minting them**. This is
#' the third linkage mode -- distinct from the initial batch build ([link_panel()],
#' all record-pairs within an org) and cross-org interlock ([link_cross_org()],
#' profiles across orgs); see `dev/PLAN-incremental-waves.md`.
#'
#' The wave is first linked **internally** ([link_panel()] over `new` alone), so a
#' first-time person appearing across several wave years becomes one wave-person;
#' the wave-people are then matched to the existing people
#' ([match_to_profiles()]). A matched wave-person **inherits** the existing
#' `EMP_ID` and `EMP_ANCHOR` -- which is what freezes an id across a backfill: an
#' earlier record for a known person takes that person's existing anchor rather
#' than re-anchoring. An unmatched wave-person keeps its freshly minted anchored
#' wave-local id (a genuinely new person).
#'
#' `new` rows whose native `(OBJECTID, TABLE_ID)` key already appears in `existing`
#' are exact re-loads, not new data; they are dropped with a warning so they do
#' not spawn duplicate people.
#'
#' @param existing A panel already linked under the anchored scheme: must carry
#'   `EMP_ID` **and** `EMP_ANCHOR` (run [link_panel()], or migrate a legacy panel
#'   with [remint_anchored()], first).
#' @param new The new wave: parsed records in the same schema as `existing`, with
#'   no ids yet.
#' @param cols Column mapping; see [synthid_cols()].
#' @param weights Base match weights; see [default_weights()].
#' @param method Wave-internal linkage method, passed to [link_panel()].
#' @param link_threshold Threshold for the wave-internal [link_panel()]
#'   (`method = "weighted"`).
#' @param match_threshold Threshold for the wave<->existing match
#'   ([match_to_profiles()]); default `7`, tuned (see that function and
#'   `dev/NOTES-match-threshold.md`).
#' @param prob_threshold Posterior threshold for the wave-internal link when
#'   `method = "em"`.
#' @param surname_weight Optional precomputed surname-rarity vector for the match
#'   stage; see [match_to_profiles()].
#' @param max_block_size Oversize-bucket guard for the match blocker.
#' @param verbose Print progress.
#' @return A list:
#'   \describe{
#'     \item{`new_stamped`}{The genuinely-new wave rows (re-loads removed) with
#'       `EMP_ID` and `EMP_ANCHOR` stamped -- inherited for returning people,
#'       freshly minted for first-time people. `rbind()` onto `existing` (after
#'       aligning columns) to grow the panel; recompute any per-person counts on
#'       the merged result.}
#'     \item{`review`}{Ambiguous / invariant-collision pairs from the match, for
#'       eyeballing (see [match_to_profiles()]); never auto-merged.}
#'     \item{`unmatched`}{Wave-local `EMP_ID`s judged first-time people.}
#'     \item{`wave_id_map`}{`wave` (wave-local id) -> `final_id`, `final_anchor`,
#'       `matched` -- the audit trail of every wave-person's resolution.}
#'     \item{`report`}{Counts for the run.}
#'   }
#' @examples
#' \dontrun{
#' existing <- link_panel(panel_2019_2023)       # anchored ids + EMP_ANCHOR
#' wave     <- readr::read_csv("PANEL-2024.csv")
#' res <- link_incremental(existing, wave)
#' merged <- rbind(existing[names(res$new_stamped)], res$new_stamped)
#' subset(res$review, reason == "ambiguous")      # spot-check borderline matches
#' }
#' @seealso [match_to_profiles()], [link_panel()], [remint_anchored()]
#' @export
link_incremental <- function(existing, new, cols = synthid_cols(),
                             weights = default_weights(),
                             method = c("weighted", "em"),
                             link_threshold = 7, match_threshold = 7,
                             prob_threshold = 0.5, surname_weight = NULL,
                             max_block_size = 5000L, verbose = FALSE) {
  say <- function(...) if (verbose) message(...)
  method <- match.arg(method)
  stopifnot(is.data.frame(existing), is.data.frame(new))
  if (!"EMP_ID" %in% names(existing)) {
    stop("link_incremental(): `existing` must be linked first (needs EMP_ID); ",
         "run link_panel().", call. = FALSE)
  }
  if (!"EMP_ANCHOR" %in% names(existing)) {
    stop("link_incremental(): `existing` lacks EMP_ANCHOR (anchored id scheme). ",
         "Re-mint it with remint_anchored() first.", call. = FALSE)
  }

  ## Drop exact re-loads: wave rows whose native key already exists upstream.
  n_reload <- 0L
  if (all(c(cols$object_id, cols$table_id) %in% names(existing)) &&
      all(c(cols$object_id, cols$table_id) %in% names(new))) {
    ekey <- paste(existing[[cols$object_id]], existing[[cols$table_id]], sep = "\r")
    nkey <- paste(new[[cols$object_id]], new[[cols$table_id]], sep = "\r")
    reload <- nkey %in% ekey
    n_reload <- sum(reload)
    if (n_reload > 0L) {
      warning(n_reload, " wave row(s) share an (OBJECTID, TABLE_ID) with `existing` ",
              "(exact re-loads); dropping them so they do not spawn duplicate people.",
              call. = FALSE)
      new <- new[!reload, , drop = FALSE]
    }
  }

  empty_map <- data.frame(wave = character(0), final_id = character(0),
                          final_anchor = character(0), matched = logical(0),
                          stringsAsFactors = FALSE)
  if (nrow(new) == 0L) {
    new_stamped <- new
    new_stamped$EMP_ID <- character(0); new_stamped$EMP_ANCHOR <- character(0)
    empty_review <- data.frame(wave_emp_id = character(0), existing_emp_id = character(0),
                               score = numeric(0), reason = character(0),
                               stringsAsFactors = FALSE)
    return(list(new_stamped = new_stamped, review = empty_review,
                unmatched = character(0), wave_id_map = empty_map,
                report = list(n_new_rows = 0L, n_reload_rows_dropped = n_reload)))
  }

  say("Profiling existing panel (", nrow(existing), " rows)...")
  existing_prof <- build_person_profile(existing, cols = cols)

  say("Linking the wave internally (", nrow(new), " rows)...")
  wave_linked <- link_panel(new, cols = cols, weights = weights,
                            threshold = link_threshold, method = method,
                            prob_threshold = prob_threshold, verbose = verbose)
  wave_prof <- build_person_profile(wave_linked, cols = cols)

  say("Matching wave-people to existing people...")
  m <- match_to_profiles(wave_prof, existing_prof, weights = weights,
                         threshold = match_threshold, surname_weight = surname_weight,
                         max_block_size = max_block_size, verbose = verbose)

  ## Resolve each wave-local person to a final id/anchor: inherit if matched,
  ## else keep the freshly minted wave-local id (a first-time person).
  map <- data.frame(wave = wave_prof$EMP_ID, final_id = wave_prof$EMP_ID,
                    final_anchor = wave_prof$EMP_ANCHOR, matched = FALSE,
                    stringsAsFactors = FALSE)
  if (nrow(m$matched)) {
    j <- match(m$matched$wave_emp_id, map$wave)
    map$final_id[j] <- m$matched$existing_emp_id
    map$final_anchor[j] <- m$matched$existing_emp_anchor
    map$matched[j] <- TRUE
  }

  pos <- match(wave_linked$EMP_ID, map$wave)
  new_stamped <- new
  new_stamped$EMP_ID <- map$final_id[pos]
  new_stamped$EMP_ANCHOR <- map$final_anchor[pos]

  report <- c(m$report, list(
    n_new_rows = nrow(new), n_reload_rows_dropped = n_reload,
    n_rows_returning = sum(map$matched[pos]),
    n_rows_first_time = sum(!map$matched[pos]),
    link_method = method))

  list(new_stamped = new_stamped, review = m$review, unmatched = m$unmatched,
       wave_id_map = map, report = report)
}
