#' Genome-wide association study (GWAS)
#'
#' Mixed-model GWAS scan using `rrBLUP::GWAS`. Requires the `rrBLUP` package.
#'
#' @param pheno Data frame: first column genotype id, remaining trait columns.
#' @param geno Data frame of markers: columns = marker, chrom, pos, then
#'   one column per genotype coded -1/0/1.
#' @param trait Trait column name in `pheno`.
#' @param ... Passed to [rrBLUP::GWAS()].
#' @return A data frame of marker -log10(p) scores.
#' @export
pb_gwas <- function(pheno, geno, trait, ...) {
  if (!requireNamespace("rrBLUP", quietly = TRUE))
    stop("Install the 'rrBLUP' package to run pb_gwas().")
  rrBLUP::GWAS(pheno = pheno[, c(names(pheno)[1], trait)],
               geno = geno, ...)
}

#' QTL mapping (interval mapping)
#'
#' Wrapper around `qtl::scanone` for single-QTL genome scans. Requires `qtl`.
#'
#' @param cross A `qtl` cross object (read via [qtl::read.cross()]).
#' @param method Scan method, e.g. "em", "hk".
#' @param pheno.col Phenotype column index.
#' @return A `scanone` result object.
#' @export
pb_qtl <- function(cross, method = "hk", pheno.col = 1) {
  if (!requireNamespace("qtl", quietly = TRUE))
    stop("Install the 'qtl' package to run pb_qtl().")
  cross <- qtl::calc.genoprob(cross, step = 1)
  qtl::scanone(cross, method = method, pheno.col = pheno.col)
}

#' Molecular diversity analysis
#'
#' Computes a marker-based distance matrix and neighbour-joining / hierarchical
#' clustering from a genotype x marker matrix (coded numerically).
#'
#' @param geno_matrix Numeric matrix, rows = genotypes, cols = markers.
#' @param method Distance method.
#' @param clusters Number of groups to cut.
#' @return A list with distance, clustering and group membership.
#' @export
pb_molecular_diversity <- function(geno_matrix, method = "manhattan", clusters = 3) {
  d <- stats::dist(geno_matrix, method = method)
  hc <- stats::hclust(d, method = "average")
  grp <- stats::cutree(hc, k = clusters)
  list(distance = d, hclust = hc, groups = grp)
}

#' Envirotyping analysis
#'
#' Summarises environmental covariables per environment and computes an
#' environmental relationship (kinship-like) matrix for enviromics.
#'
#' @param env_data Data frame: environment id column + environmental covariates.
#' @param env Character name of the environment column.
#' @return A list with scaled covariates and the environmental relationship matrix.
#' @export
pb_envirotype <- function(env_data, env) {
  covars <- env_data[, setdiff(names(env_data), env), drop = FALSE]
  W <- scale(as.matrix(covars))
  rownames(W) <- env_data[[env]]
  E <- tcrossprod(W) / ncol(W)   # environmental relationship matrix
  list(covariates = W, env_relationship = E)
}

#' Bayesian genomic analysis
#'
#' Fits a Bayesian genomic regression (e.g. BayesB / BRR) via the `BGLR`
#' package. Requires `BGLR`.
#'
#' @param y Numeric response vector.
#' @param geno Marker matrix (genotypes x markers).
#' @param model BGLR model type: "BRR", "BayesA", "BayesB", "BayesC", "BL".
#' @param nIter,burnIn MCMC controls.
#' @return A fitted `BGLR` object.
#' @export
pb_bayesian <- function(y, geno, model = "BayesB", nIter = 5000, burnIn = 1000) {
  if (!requireNamespace("BGLR", quietly = TRUE))
    stop("Install the 'BGLR' package to run pb_bayesian().")
  ETA <- list(list(X = geno, model = model))
  BGLR::BGLR(y = y, ETA = ETA, nIter = nIter, burnIn = burnIn, verbose = FALSE)
}
