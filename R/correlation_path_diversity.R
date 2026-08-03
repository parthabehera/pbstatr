#' Correlation analysis among traits
#'
#' Computes phenotypic (and optionally genotypic) correlation matrices.
#'
#' @param data A data frame.
#' @param traits Character vector of trait column names.
#' @param method Correlation method passed to [stats::cor()].
#' @return A correlation matrix.
#' @export
pb_correlation <- function(data, traits, method = "pearson") {
  m <- as.matrix(data[, traits, drop = FALSE])
  stats::cor(m, use = "pairwise.complete.obs", method = method)
}

#' Path coefficient analysis
#'
#' Decomposes correlations into direct and indirect effects via Wright's
#' path analysis, using a set of causal traits on one dependent trait.
#'
#' @param data A data frame.
#' @param dependent Character name of the effect (response) trait.
#' @param causal Character vector of cause (predictor) traits.
#' @return A list with the matrix of direct effects, indirect effects and residual.
#' @export
pb_path_analysis <- function(data, dependent, causal) {
  R <- stats::cor(data[, causal, drop = FALSE], use = "pairwise.complete.obs")
  r_y <- stats::cor(data[, causal, drop = FALSE], data[[dependent]],
                    use = "pairwise.complete.obs")
  direct <- solve(R, r_y)                     # path coefficients
  indirect <- outer(as.vector(direct), rep(1, length(causal))) * 0
  indirect <- sweep(R, 1, as.vector(direct), "*")
  diag(indirect) <- 0
  residual <- sqrt(max(0, 1 - sum(direct * r_y)))
  list(
    direct = setNames(as.vector(direct), causal),
    indirect = indirect,
    correlation_with_dependent = setNames(as.vector(r_y), causal),
    residual = residual
  )
}

#' Genetic diversity analysis
#'
#' Computes a distance matrix and hierarchical clustering (and optionally
#' Mahalanobis D2 style analysis) for genotype grouping.
#'
#' @param data A data frame with genotypes in rows.
#' @param traits Character vector of trait columns.
#' @param id Character name of the genotype id column.
#' @param method Distance method for [stats::dist()].
#' @param clusters Number of clusters to cut the dendrogram into.
#' @return A list with the distance matrix, hclust object and cluster membership.
#' @export
pb_diversity <- function(data, traits, id = NULL, method = "euclidean", clusters = 3) {
  X <- scale(as.matrix(data[, traits, drop = FALSE]))
  if (!is.null(id)) rownames(X) <- data[[id]]
  d <- stats::dist(X, method = method)
  hc <- stats::hclust(d, method = "ward.D2")
  grp <- stats::cutree(hc, k = clusters)
  list(distance = d, hclust = hc, groups = grp)
}
