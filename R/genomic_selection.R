#' Mixed linear model with BLUPs
#'
#' Fits a mixed model with genotype as a random effect (and optional block /
#' environment terms) via `lme4`, returning BLUPs, variance components and
#' broad-sense heritability on an entry-mean basis.
#'
#' @param data A data frame.
#' @param trait Response trait column name.
#' @param genotype Genotype column name (fitted as random).
#' @param fixed Character vector of fixed-effect terms (e.g. "env").
#' @param random Character vector of additional random terms (e.g. "env:rep").
#' @return A list with the model, BLUPs, variance components and heritability.
#' @export
pb_blup <- function(data, trait, genotype, fixed = NULL, random = NULL) {
  terms_fixed <- if (is.null(fixed)) "1" else paste(fixed, collapse = " + ")
  rand <- c(sprintf("(1|%s)", genotype))
  if (!is.null(random)) rand <- c(rand, sprintf("(1|%s)", random))
  fml <- stats::as.formula(
    paste(trait, "~", terms_fixed, "+", paste(rand, collapse = " + ")))
  model <- lme4::lmer(fml, data = data)

  vc <- as.data.frame(lme4::VarCorr(model))
  vg <- vc$vcov[vc$grp == genotype]
  ve <- vc$vcov[vc$grp == "Residual"]
  reps <- max(table(data[[genotype]]))
  h2 <- vg / (vg + ve / reps)          # entry-mean heritability

  re <- lme4::ranef(model)[[genotype]]
  blups <- data.frame(genotype = rownames(re),
                      BLUP = re[, 1] + lme4::fixef(model)[1],
                      effect = re[, 1], row.names = NULL)
  blups <- blups[order(-blups$BLUP), ]

  list(model = model, blups = blups,
       var_components = vc,
       heritability = h2)
}

#' Genomic relationship matrix (VanRaden method 1)
#'
#' @param M Marker matrix (genotypes x markers) coded 0/1/2 or -1/0/1.
#' @return A genotypes x genotypes genomic relationship matrix (G).
#' @export
pb_grm <- function(M) {
  M <- as.matrix(M)
  if (min(M, na.rm = TRUE) >= 0) M <- M - 1   # recentre 0/1/2 -> -1/0/1
  p <- (colMeans(M, na.rm = TRUE) + 1) / 2      # allele freq
  P <- matrix(2 * (p - 0.5), nrow = nrow(M), ncol = ncol(M), byrow = TRUE)
  Z <- M - P
  denom <- 2 * sum(p * (1 - p))
  G <- tcrossprod(Z) / denom
  diag(G) <- diag(G) + 1e-6                      # ensure invertibility
  G
}

#' Genomic prediction / GBLUP
#'
#' Genomic BLUP using `rrBLUP::mixed.solve` on a marker matrix, returning
#' GEBVs and marker effects. Optionally runs k-fold cross-validation to report
#' predictive ability.
#'
#' @param y Numeric phenotype vector (NA allowed for prediction candidates).
#' @param M Marker matrix (genotypes x markers), rows aligned to `y`.
#' @param cv_folds If > 1, run k-fold CV and report mean predictive ability.
#' @param seed RNG seed for CV.
#' @return A list with GEBVs, marker effects and (optionally) CV accuracy.
#' @export
pb_gblup <- function(y, M, cv_folds = 0, seed = 1) {
  if (!requireNamespace("rrBLUP", quietly = TRUE))
    stop("Install the 'rrBLUP' package to run pb_gblup().")
  M <- as.matrix(M)
  fit <- rrBLUP::mixed.solve(y = y, Z = M)
  gebv <- as.vector(M %*% fit$u)
  out <- list(marker_effects = fit$u, GEBV = gebv,
              Vu = fit$Vu, Ve = fit$Ve,
              heritability = fit$Vu / (fit$Vu + fit$Ve))

  if (cv_folds > 1) {
    set.seed(seed)
    obs <- which(!is.na(y))
    folds <- sample(rep(seq_len(cv_folds), length.out = length(obs)))
    acc <- numeric(cv_folds)
    for (k in seq_len(cv_folds)) {
      test <- obs[folds == k]
      ytrain <- y; ytrain[test] <- NA
      f <- rrBLUP::mixed.solve(y = ytrain, Z = M)
      pred <- as.vector(M[test, , drop = FALSE] %*% f$u)
      acc[k] <- stats::cor(pred, y[test], use = "complete.obs")
    }
    out$cv_accuracy <- mean(acc)
    out$cv_fold_accuracy <- acc
  }
  out
}

#' Selection: pick top genotypes by predicted merit
#'
#' @param values Named numeric vector of BLUPs/GEBVs (names = genotype ids).
#' @param proportion Selection proportion (0-1) OR use `n`.
#' @param n Number to select (overrides `proportion`).
#' @param intensity Return the standardised selection intensity too.
#' @return A list with selected ids, threshold and selection differential.
#' @export
pb_select <- function(values, proportion = 0.1, n = NULL, intensity = TRUE) {
  ord <- sort(values, decreasing = TRUE)
  k <- if (!is.null(n)) n else ceiling(length(values) * proportion)
  selected <- ord[seq_len(k)]
  diff <- mean(selected) - mean(values)
  i <- if (intensity && stats::sd(values) > 0) diff / stats::sd(values) else NA
  list(selected = selected, n_selected = k,
       threshold = min(selected),
       selection_differential = diff,
       selection_intensity = i)
}
