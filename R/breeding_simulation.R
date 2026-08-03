#' Found a base breeding population (AlphaSimR)
#'
#' Simulates founder haplotypes and a base population with an additive-trait
#' genetic architecture using `AlphaSimR`.
#'
#' @param n_founders Number of founder individuals.
#' @param n_chr Number of chromosomes.
#' @param seg_sites Segregating sites per chromosome.
#' @param n_qtl QTL per chromosome for the trait.
#' @param mean Trait mean.
#' @param var Additive genetic variance.
#' @param h2 Narrow-sense heritability for phenotyping.
#' @return A list with the `SimParam` object and the base population.
#' @export
pb_sim_founders <- function(n_founders = 100, n_chr = 10, seg_sites = 1000,
                            n_qtl = 100, mean = 100, var = 10, h2 = 0.4) {
  if (!requireNamespace("AlphaSimR", quietly = TRUE))
    stop("Install the 'AlphaSimR' package to run the breeding simulation.")
  founderPop <- AlphaSimR::runMacs(nInd = n_founders, nChr = n_chr,
                                   segSites = seg_sites)
  SP <- AlphaSimR::SimParam$new(founderPop)
  SP$addTraitA(nQtlPerChr = n_qtl, mean = mean, var = var)
  SP$setVarE(h2 = h2)
  pop <- AlphaSimR::newPop(founderPop, simParam = SP)
  list(SP = SP, pop = pop)
}

#' Run a recurrent selection breeding pipeline (AlphaSimR)
#'
#' Simulates cycles of phenotypic, genomic, or truncation selection followed by
#' random mating, tracking genetic gain and genetic variance across cycles.
#'
#' @param sim Output of [pb_sim_founders()].
#' @param cycles Number of breeding cycles.
#' @param n_select Individuals selected each cycle.
#' @param n_cross Crosses (progeny) made each cycle.
#' @param method "phenotypic", "genomic", or "random".
#' @return A list with per-cycle summary (mean genetic value, variance) and the
#'   final population.
#' @export
pb_sim_pipeline <- function(sim, cycles = 10, n_select = 20, n_cross = 100,
                            method = c("phenotypic", "genomic", "random")) {
  if (!requireNamespace("AlphaSimR", quietly = TRUE))
    stop("Install the 'AlphaSimR' package to run the breeding simulation.")
  method <- match.arg(method)
  SP <- sim$SP
  pop <- sim$pop

  track <- data.frame(cycle = 0:cycles, meanG = NA_real_, varG = NA_real_)
  track$meanG[1] <- AlphaSimR::meanG(pop)
  track$varG[1]  <- AlphaSimR::varG(pop)[1]

  for (cy in seq_len(cycles)) {
    sel <- switch(method,
      phenotypic = AlphaSimR::selectInd(pop, nInd = n_select, use = "pheno",
                                        simParam = SP),
      genomic    = AlphaSimR::selectInd(pop, nInd = n_select, use = "ebv",
                                        simParam = SP),
      random     = AlphaSimR::selectInd(pop, nInd = n_select, use = "rand",
                                        simParam = SP)
    )
    pop <- AlphaSimR::randCross(sel, nCrosses = n_cross, simParam = SP)
    track$meanG[cy + 1] <- AlphaSimR::meanG(pop)
    track$varG[cy + 1]  <- AlphaSimR::varG(pop)[1]
  }

  gain <- track$meanG[cycles + 1] - track$meanG[1]
  list(summary = track, total_gain = gain,
       gain_per_cycle = gain / cycles, final_pop = pop)
}

#' Plot genetic gain from a simulated pipeline
#'
#' @param pipeline Output of [pb_sim_pipeline()].
#' @return A `ggplot` object of mean genetic value across cycles.
#' @export
pb_plot_gain <- function(pipeline) {
  df <- pipeline$summary
  ggplot2::ggplot(df, ggplot2::aes(cycle, meanG)) +
    ggplot2::geom_line(color = "#2E9FDF", linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(x = "Breeding cycle", y = "Mean genetic value",
                  title = "Simulated genetic gain") +
    ggplot2::theme_minimal()
}
