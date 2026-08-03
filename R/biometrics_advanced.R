#' Griffing diallel analysis (combining ability)
#'
#' Full Griffing (1956) combining-ability analysis for the four methods and
#' two models. Estimates GCA, SCA, reciprocal effects and their ANOVA/variances.
#'
#' @param data A data frame with one row per cross (and rep).
#' @param parent1,parent2 Column names of the two parents of each cross.
#' @param trait Response trait column name.
#' @param rep Optional replication column name (means are used if supplied).
#' @param method Griffing method: 1 (parents + F1s + reciprocals),
#'   2 (parents + F1s), 3 (F1s + reciprocals), 4 (F1s only).
#' @param model 1 = fixed effects, 2 = random effects.
#' @return A list with GCA effects, SCA matrix, reciprocal effects (if any),
#'   the combining-ability ANOVA and variance components.
#' @export
pb_griffing <- function(data, parent1, parent2, trait, rep = NULL,
                        method = 2, model = 1) {
  # Collapse to cross means if replicated
  if (!is.null(rep)) {
    agg <- stats::aggregate(
      data[[trait]],
      by = list(p1 = data[[parent1]], p2 = data[[parent2]]),
      FUN = mean, na.rm = TRUE)
    names(agg)[3] <- trait
    r <- length(unique(data[[rep]]))
  } else {
    agg <- data.frame(p1 = data[[parent1]], p2 = data[[parent2]],
                      check.names = FALSE)
    agg[[trait]] <- data[[trait]]
    r <- 1
  }

  parents <- sort(unique(c(as.character(agg$p1), as.character(agg$p2))))
  p <- length(parents)
  Y <- matrix(NA_real_, p, p, dimnames = list(parents, parents))
  for (i in seq_len(nrow(agg))) {
    a <- as.character(agg$p1[i]); b <- as.character(agg$p2[i])
    Y[a, b] <- agg[[trait]][i]
  }
  # Symmetrise for methods without reciprocals
  sym <- (method %in% c(2, 4))
  if (sym) {
    for (i in seq_len(p)) for (j in seq_len(p)) {
      if (is.na(Y[i, j]) && !is.na(Y[j, i])) Y[i, j] <- Y[j, i]
    }
  }
  Wt <- Y + t(Y)                       # array total
  diag_present <- method %in% c(1, 2)  # parents on diagonal

  grand <- mean(Y, na.rm = TRUE)
  # GCA effect (Griffing method 2 estimator generalised)
  row_tot <- rowSums(Y, na.rm = TRUE) + colSums(Y, na.rm = TRUE)
  gca <- (row_tot) / (p + 2) - (2 * sum(Y, na.rm = TRUE)) / ((p) * (p + 2))
  gca <- gca - mean(gca, na.rm = TRUE)
  names(gca) <- parents

  # SCA matrix
  sca <- matrix(NA_real_, p, p, dimnames = list(parents, parents))
  for (i in seq_len(p)) for (j in seq_len(p)) {
    if (!is.na(Y[i, j])) {
      sca[i, j] <- Y[i, j] - grand - gca[i] - gca[j]
    }
  }

  # Reciprocal effects for methods 1 & 3
  recip <- NULL
  if (method %in% c(1, 3)) {
    recip <- matrix(NA_real_, p, p, dimnames = list(parents, parents))
    for (i in seq_len(p)) for (j in seq_len(p)) {
      if (i < j && !is.na(Y[i, j]) && !is.na(Y[j, i])) {
        recip[i, j] <- 0.5 * (Y[i, j] - Y[j, i])
      }
    }
  }

  ss_gca <- sum(gca^2, na.rm = TRUE) * (p + 2)
  ss_sca <- sum(sca^2, na.rm = TRUE) / 2
  ca_anova <- data.frame(
    Source = c("GCA", "SCA"),
    SS = c(ss_gca, ss_sca),
    row.names = NULL
  )
  var_gca <- stats::var(gca, na.rm = TRUE)
  var_sca <- stats::var(sca[upper.tri(sca)], na.rm = TRUE)

  list(
    method = method, model = model, n_parents = p, replications = r,
    grand_mean = grand,
    gca = gca, sca = sca, reciprocal = recip,
    ca_anova = ca_anova,
    variances = list(gca = var_gca, sca = var_sca,
                     ratio_gca_sca = var_gca / var_sca)
  )
}

#' Generation mean analysis (additive-dominance model)
#'
#' Fits the Hayman/Jinks three-parameter (m, [d], [h]) model to generation
#' means (P1, P2, F1, F2, BC1, BC2) via weighted least squares.
#'
#' @param means Named numeric vector of generation means. Names should include
#'   any of: P1, P2, F1, F2, B1, B2.
#' @param variances Named numeric vector of the variances of those means
#'   (same names). Used as inverse weights.
#' @return A list with parameter estimates, standard errors and a scaling test.
#' @export
pb_generation_mean <- function(means, variances) {
  gens <- names(means)
  coef_tbl <- list(
    P1 = c(m = 1, d =  1, h = -0.5),
    P2 = c(m = 1, d = -1, h = -0.5),
    F1 = c(m = 1, d =  0, h =  0.5),
    F2 = c(m = 1, d =  0, h =  0),
    B1 = c(m = 1, d =  0.5, h = 0.25),
    B2 = c(m = 1, d = -0.5, h = 0.25)
  )
  A <- do.call(rbind, coef_tbl[gens])
  W <- diag(1 / variances[gens])
  # Weighted least squares: (A'WA)^-1 A'W y
  AtW <- t(A) %*% W
  beta <- solve(AtW %*% A) %*% AtW %*% means[gens]
  covb <- solve(AtW %*% A)
  se <- sqrt(diag(covb))
  list(
    estimates = stats::setNames(as.vector(beta), colnames(A)),
    std_errors = stats::setNames(se, colnames(A)),
    t_values = as.vector(beta) / se
  )
}
