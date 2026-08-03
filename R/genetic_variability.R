#' Analysis of Variance for a designed experiment
#'
#' Wrapper around [stats::aov()] returning tidy ANOVA tables and mean squares
#' needed for downstream genetic parameter estimation.
#'
#' @param data A data frame.
#' @param trait Character name of the response variable.
#' @param genotype Character name of the genotype/treatment factor.
#' @param block Character name of the block/replication factor. Optional.
#' @param design One of "crd" or "rcbd".
#' @return A list with the fitted model, the ANOVA table, and extracted mean squares.
#' @export
#' @examples
#' df <- data.frame(
#'   gen = rep(paste0("G", 1:5), each = 3),
#'   rep = rep(1:3, times = 5),
#'   yld = rnorm(15, 50, 5)
#' )
#' pb_anova(df, "yld", "gen", "rep", design = "rcbd")
pb_anova <- function(data, trait, genotype, block = NULL, design = c("rcbd", "crd")) {
  design <- match.arg(design)
  data[[genotype]] <- as.factor(data[[genotype]])
  if (design == "rcbd") {
    if (is.null(block)) stop("`block` is required for an RCBD.")
    data[[block]] <- as.factor(data[[block]])
    fml <- stats::as.formula(paste(trait, "~", genotype, "+", block))
  } else {
    fml <- stats::as.formula(paste(trait, "~", genotype))
  }
  model <- stats::aov(fml, data = data)
  tab <- as.data.frame(stats::anova(model))

  reps <- if (!is.null(block)) length(unique(data[[block]])) else {
    max(table(data[[genotype]]))
  }
  ms_gen <- tab[genotype, "Mean Sq"]
  ms_err <- tab["Residuals", "Mean Sq"]

  list(
    model = model,
    anova = tab,
    mean_squares = list(genotype = ms_gen, error = ms_err),
    replications = reps
  )
}

#' Genetic variability parameters
#'
#' Estimates genotypic and phenotypic variances and derived parameters:
#' GCV, PCV, ECV, broad-sense heritability, genetic advance (GA) and
#' genetic advance as percent of mean (GAM).
#'
#' @param data A data frame.
#' @param trait Character name of the response variable.
#' @param genotype Character name of the genotype factor.
#' @param block Character name of the block factor.
#' @param k Selection differential (default 2.063 for 5% selection intensity).
#' @return A data frame of genetic parameters for the trait.
#' @export
pb_genetic_params <- function(data, trait, genotype, block, k = 2.063) {
  fit <- pb_anova(data, trait, genotype, block, design = "rcbd")
  r <- fit$replications
  ms_g <- fit$mean_squares$genotype
  ms_e <- fit$mean_squares$error

  vg <- (ms_g - ms_e) / r          # genotypic variance
  vg <- max(vg, 0)
  ve <- ms_e                        # environmental variance
  vp <- vg + ve                     # phenotypic variance
  gm <- mean(data[[trait]], na.rm = TRUE)

  gcv <- 100 * sqrt(vg) / gm
  pcv <- 100 * sqrt(vp) / gm
  ecv <- 100 * sqrt(ve) / gm
  h2  <- if (vp > 0) vg / vp else NA_real_   # broad-sense heritability
  ga  <- k * sqrt(vp) * h2
  gam <- 100 * ga / gm

  data.frame(
    Trait = trait, Mean = gm,
    Vg = vg, Ve = ve, Vp = vp,
    GCV = gcv, PCV = pcv, ECV = ecv,
    Heritability = h2, GA = ga, GAM = gam,
    row.names = NULL
  )
}
