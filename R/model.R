#' All candidate pairs with their per-field comparison vectors
#'
#' Like [candidate_scores()], but returns the raw per-field similarity columns
#' (the graded comparison vector) rather than a single combined score. This is the
#' substrate for fitting a match model ([fit_match_model()]).
#'
#' @param df A raw panel (as for [link_panel()]).
#' @param cols Column mapping; see [synthid_cols()].
#' @param features Feature columns to compare (defaults to the standard set).
#' @return A data frame, one row per candidate pair: `row_x`, `row_y`, `yr_x`,
#'   `yr_y`, `org`, `surname_w_x`, `surname_w_y`, and one numeric similarity column
#'   per feature (`NA` where a field was missing on either side).
#' @export
candidate_comparisons <- function(df, cols = synthid_cols(),
                                  features = NULL) {
  prep <- prepare_panel(df, cols)
  work <- prep$work
  feats <- if (is.null(features)) prep$features else intersect(features, names(work))
  comparators <- match_comparators(feats)
  years <- sort(unique(work$.year))
  out <- vector("list", 0L)

  for (i in seq_along(years)) {
    for (j in seq_along(years)) {
      if (j <= i) next
      dx <- work[work$.year == years[i], , drop = FALSE]
      dy <- work[work$.year == years[j], , drop = FALSE]
      if (nrow(dx) == 0L || nrow(dy) == 0L) next
      pairs <- reclin2::pair_blocking(dx, dy, on = ".org")
      if (nrow(pairs) == 0L) next
      pairs <- reclin2::compare_pairs(
        pairs, on = feats, comparators = comparators,
        default_comparator = reclin2::cmp_jarowinkler(), inplace = FALSE
      )
      pdf <- as.data.frame(pairs)
      rec <- data.frame(
        row_x = dx$.row_uid[pdf$.x], row_y = dy$.row_uid[pdf$.y],
        yr_x = years[i], yr_y = years[j], org = dx$.org[pdf$.x],
        surname_w_x = dx$.surname_w[pdf$.x], surname_w_y = dy$.surname_w[pdf$.y],
        stringsAsFactors = FALSE
      )
      for (f in feats) rec[[f]] <- pdf[[f]]
      out[[length(out) + 1L]] <- rec
    }
  }
  if (length(out) == 0L) return(data.frame())
  attr_feats <- feats
  res <- do.call(rbind, out)
  attr(res, "features") <- attr_feats
  res
}

#' The feature (comparison) columns of a comparisons data frame
#' @keywords internal
model_features <- function(comparisons) {
  attr(comparisons, "features") %||%
    setdiff(names(comparisons),
            c("row_x", "row_y", "yr_x", "yr_y", "org",
              "surname_w_x", "surname_w_y"))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Fit a match model to candidate comparison vectors
#'
#' Two methods, both returning an object usable by [predict_match()] to emit a
#' calibrated per-pair match probability (`p_match`), the confidence signal the
#' downstream panel model wants:
#' \describe{
#'   \item{`"em"` (default, unsupervised)}{Fellegi-Sunter latent-class model.
#'     Each field is binarised (similarity `>= agree_cutoff` is agreement) and
#'     [reclin2::problink_em()] learns, by EM with **no labels**, the probability
#'     of agreement among true matches (`m`) and among non-matches (`u`) plus the
#'     prior match rate. Weights become learned rather than hand-set; the posterior
#'     is the naive-Bayes combination of the per-field log likelihood ratios.}
#'   \item{`"logistic"` (supervised)}{Logistic regression of the label on the
#'     graded (not binarised) similarities, so it uses the full resolution of the
#'     comparators. Missing fields are mean-imputed with a missingness indicator.
#'     Requires `labels`.}
#' }
#'
#' @param comparisons Output of [candidate_comparisons()].
#' @param method `"em"` or `"logistic"`.
#' @param labels Integer/logical vector of length `nrow(comparisons)` (`1`/`0`,
#'   `NA` to ignore) — required for `"logistic"`, ignored for `"em"`.
#' @param agree_cutoff Similarity at or above which a field counts as agreement
#'   (EM only).
#' @return An object of class `synthid_model`.
#' @examples
#' \dontrun{
#' cmp <- candidate_comparisons(panel)
#' em  <- fit_match_model(cmp)          # unsupervised
#' fs_weights(em)                       # learned agreement/disagreement weights
#' }
#' @export
fit_match_model <- function(comparisons, method = c("em", "logistic"),
                            labels = NULL, agree_cutoff = 0.5) {
  method <- match.arg(method)
  feats <- model_features(comparisons)
  X <- comparisons[feats]

  if (method == "em") {
    G <- as.matrix(X) >= agree_cutoff        # TRUE/FALSE/NA agreement per field
    em <- em_fellegi_sunter(G)
    obj <- list(method = "em", feats = feats, agree_cutoff = agree_cutoff,
                m = em$m, u = em$u, p = em$p, iters = em$iters)
  } else {
    if (is.null(labels)) stop("method = 'logistic' requires labels.", call. = FALSE)
    if (length(labels) != nrow(comparisons)) {
      stop("labels must have length nrow(comparisons).", call. = FALSE)
    }
    keep <- !is.na(labels)
    Xl <- X[keep, , drop = FALSE]
    dat <- data.frame(.y = as.integer(labels[keep]))
    terms <- character(0)
    for (f in feats) {
      v <- Xl[[f]]; na <- is.na(v); v[na] <- 0.5
      dat[[f]] <- v; terms <- c(terms, f)
      if (any(na)) { dat[[paste0(f, "_na")]] <- as.integer(na); terms <- c(terms, paste0(f, "_na")) }
    }
    fit <- stats::glm(.y ~ ., data = dat, family = stats::binomial())
    obj <- list(method = "logistic", feats = feats, fit = fit, terms = terms)
  }
  class(obj) <- "synthid_model"
  obj
}

#' Fellegi-Sunter latent-class EM (unsupervised)
#'
#' Fits the classic two-class (match / non-match) latent model to a matrix of
#' binary field agreements under the conditional-independence assumption,
#' learning by EM -- with no labels -- the prior match rate `p` and, per field,
#' the agreement probability among matches (`m`) and non-matches (`u`). This is
#' the model \code{reclin2::problink_em()} implements; it is inlined here to work
#' directly on a plain agreement matrix. Missing agreements (`NA`) contribute no
#' evidence for that field/row.
#'
#' @param G Logical matrix (rows = pairs, cols = fields), `NA` allowed.
#' @param tol,max_iter Convergence tolerance and iteration cap.
#' @return List with `m`, `u` (named by column), `p`, and `iters`.
#' @keywords internal
em_fellegi_sunter <- function(G, tol = 1e-6, max_iter = 200) {
  feats <- colnames(G)
  Gn <- G * 1                      # 1/0/NA numeric
  obs <- !is.na(Gn)                # observed mask
  clamp <- function(x) pmin(pmax(x, 1e-4), 1 - 1e-4)

  m <- rep(0.9, ncol(Gn)); u <- rep(0.1, ncol(Gn)); p <- 0.1
  names(m) <- feats; names(u) <- feats

  ll_terms <- function(prob) {
    ## per-row sum over observed fields of G*log(prob)+(1-G)*log(1-prob)
    lp <- log(prob); lq <- log(1 - prob)
    T1 <- sweep(replace(Gn, !obs, 0), 2, lp, `*`)
    T0 <- sweep(replace(1 - Gn, !obs, 0), 2, lq, `*`)
    rowSums(T1 + T0)
  }

  iters <- 0L
  for (it in seq_len(max_iter)) {
    iters <- it
    logM <- log(p) + ll_terms(m)
    logU <- log(1 - p) + ll_terms(u)
    g <- 1 / (1 + exp(logU - logM))          # E-step posterior P(match)
    p_new <- mean(g)
    G0 <- replace(Gn, !obs, 0)
    dm <- colSums(g * obs); du <- colSums((1 - g) * obs)
    ## A field with no observed comparisons stays neutral (m = u = 0.5).
    gm <- ifelse(dm > 0, colSums(g * G0) / dm, 0.5)
    um <- ifelse(du > 0, colSums((1 - g) * G0) / du, 0.5)
    m_new <- clamp(gm); u_new <- clamp(um)
    delta <- max(abs(m_new - m), abs(u_new - u), abs(p_new - p))
    m <- m_new; u <- u_new; p <- p_new
    if (delta < tol) break
  }
  list(m = m, u = u, p = p, iters = iters)
}

#' Learned Fellegi-Sunter agreement / disagreement weights (EM models)
#'
#' @param model A `synthid_model` from [fit_match_model()] with `method = "em"`.
#' @return A data frame of `m`, `u`, and the log2 weights for agreement and
#'   disagreement per field.
#' @export
fs_weights <- function(model) {
  if (model$method != "em") stop("fs_weights() is for EM models.", call. = FALSE)
  m <- model$m; u <- model$u
  data.frame(
    feature = model$feats,
    m = round(m, 3), u = round(u, 3),
    w_agree = round(log2(m / u), 2),
    w_disagree = round(log2((1 - m) / (1 - u)), 2),
    row.names = NULL
  )
}

#' Predict a calibrated match probability for comparison vectors
#'
#' @param model A `synthid_model`.
#' @param comparisons A comparisons data frame (same features as training).
#' @return Numeric vector of `p_match` in `[0, 1]`.
#' @export
predict_match <- function(model, comparisons) {
  feats <- model$feats
  X <- comparisons[feats]
  if (model$method == "em") {
    ## Naive-Bayes posterior from the learned m/u and prior p.
    lo <- rep(log(model$p / (1 - model$p)), nrow(X))
    for (f in feats) {
      s <- X[[f]]
      agree <- s >= model$agree_cutoff
      lr <- ifelse(agree,
                   log(model$m[[f]] / model$u[[f]]),
                   log((1 - model$m[[f]]) / (1 - model$u[[f]])))
      lr[is.na(lr)] <- 0  # missing field: no evidence
      lo <- lo + lr
    }
    return(1 / (1 + exp(-lo)))
  }
  ## logistic
  dat <- data.frame(row.names = seq_len(nrow(X)))
  for (f in feats) {
    v <- X[[f]]; na <- is.na(v); v[na] <- 0.5
    dat[[f]] <- v
    if (paste0(f, "_na") %in% model$terms) dat[[paste0(f, "_na")]] <- as.integer(na)
  }
  as.numeric(stats::predict(model$fit, newdata = dat, type = "response"))
}
