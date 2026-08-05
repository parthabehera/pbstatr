#' Bayesian multi-environment analysis with ProbBreed
#'
#' Wrappers around the `ProbBreed` package (Dias et al., 2022), which uses a
#' Bayesian multi-environment model (fitted with `rstan`) to compute the
#' *probability of superior performance* and *superior stability* of genotypes
#' -- a risk-aware framework for cultivar recommendation. The typical workflow
#' is [pb_bayes_met()] -> [pb_bayes_extract()] -> [pb_bayes_prob()], then the
#' plotting helpers.
#'
#' All functions require the `ProbBreed` package (a Suggests dependency, which
#' itself needs a working `rstan`/Stan toolchain). They error with an
#' informative message if it is not installed. Model fitting is
#' computationally intensive (MCMC), so examples are wrapped in `\dontrun{}`.
#'
#' @name probbreed_bayesian
NULL

.need_probbreed <- function(fn) {
  if (!requireNamespace("ProbBreed", quietly = TRUE))
    stop("`", fn, "()` needs the 'ProbBreed' package (and a working 'rstan'). ",
         "Install it with install.packages('ProbBreed').", call. = FALSE)
}

#' Fit a Bayesian multi-environment model (ProbBreed)
#'
#' Fits one of ProbBreed's nine Bayesian MET models via `ProbBreed::bayes_met`.
#'
#' @param data A long-format data frame.
#' @param gen,loc,repl Column names for genotype, location/environment and
#'   replication (`repl` may be a vector, e.g. `c("Rep","Block")`).
#' @param trait Response trait column name.
#' @param reg,year Optional region (mega-environment) and year column names.
#' @param res_het Model heterogeneous residual variances across environments.
#' @param iter,chains,cores MCMC controls (iterations, chains, parallel cores).
#' @param ... Further arguments passed to [ProbBreed::bayes_met()].
#' @return A `stanfit` object to pass to [pb_bayes_extract()].
#' @rdname probbreed_bayesian
#' @export
pb_bayes_met <- function(data, gen, loc, repl, trait, reg = NULL, year = NULL,
                         res_het = TRUE, iter = 4000, chains = 4, cores = 1,
                         ...) {
  .need_probbreed("pb_bayes_met")
  ProbBreed::bayes_met(data = data, gen = gen, loc = loc, repl = repl,
                       trait = trait, reg = reg, year = year,
                       res.het = res_het, iter = iter, chains = chains,
                       cores = cores, verbose = FALSE, ...)
}

#' Extract posterior outputs and diagnostics (ProbBreed)
#'
#' Processes a fitted model into posterior distributions, maximum-posterior
#' values, variance components and goodness-of-fit diagnostics (WAIC, R-hat,
#' posterior predictive checks) via `ProbBreed::extr_outs`.
#'
#' @param model A `stanfit` model from [pb_bayes_met()].
#' @param probs Two probabilities for the HPD interval of variance components.
#' @param verbose Print a console summary.
#' @return An object of class `extr`.
#' @rdname probbreed_bayesian
#' @export
pb_bayes_extract <- function(model, probs = c(0.05, 0.95), verbose = FALSE) {
  .need_probbreed("pb_bayes_extract")
  ProbBreed::extr_outs(model = model, probs = probs, verbose = verbose)
}

#' Probabilities of superior performance and stability (ProbBreed)
#'
#' Computes marginal and pairwise probabilities of superior performance, and
#' the probability of superior stability, given a selection intensity, via
#' `ProbBreed::prob_sup`.
#'
#' @param extr An `extr` object from [pb_bayes_extract()].
#' @param intensity Selection intensity, as a decimal (e.g. 0.2 for 20%).
#' @param increase `TRUE` if higher trait values are better.
#' @param verbose Print a console summary.
#' @return An object of class `probsup` (lists `across` and `within`).
#' @rdname probbreed_bayesian
#' @export
pb_bayes_prob <- function(extr, intensity = 0.2, increase = TRUE,
                          verbose = FALSE) {
  .need_probbreed("pb_bayes_prob")
  ProbBreed::prob_sup(extr = extr, int = intensity, increase = increase,
                      save.df = FALSE, verbose = verbose)
}

#' Diagnostic plots for a Bayesian MET model (ProbBreed)
#'
#' The goodness-of-fit and convergence plots for an `extr` object: posterior
#' effect histograms, density overlays (real vs generated data), and trace
#' plots for MCMC convergence.
#'
#' @param extr An `extr` object from [pb_bayes_extract()].
#' @param ... Passed to `ProbBreed`'s `plot()` method for `extr` objects
#'   (e.g. `type` to choose the plot).
#' @return A `ggplot` object.
#' @rdname probbreed_bayesian
#' @export
pb_bayes_diag_plot <- function(extr, ...) {
  .need_probbreed("pb_bayes_diag_plot")
  plot(extr, ...)
}

#' Probability plots for genotype superiority (ProbBreed)
#'
#' Visualises the probabilities of superior performance / stability from a
#' `probsup` object -- the headline Bayesian figures for cultivar
#' recommendation.
#'
#' @param probsup A `probsup` object from [pb_bayes_prob()].
#' @param category Which probability to plot, passed as `category` to
#'   `ProbBreed`'s `plot.probsup` (e.g. "perfo", "stabil", "pair_perfo",
#'   "joint").
#' @param ... Further arguments to the `plot()` method.
#' @return A `ggplot` object.
#' @rdname probbreed_bayesian
#' @export
pb_bayes_prob_plot <- function(probsup, category = "perfo", ...) {
  .need_probbreed("pb_bayes_prob_plot")
  plot(probsup, category = category, ...)
}

#' Tidy the marginal probability of superior performance (across environments)
#'
#' Convenience extractor that pulls the across-environment marginal
#' probability-of-superior-performance table out of a `probsup` object and
#' returns it as a tidy, ranked data frame.
#'
#' @param probsup A `probsup` object from [pb_bayes_prob()].
#' @param what One of "performance" or "stability".
#' @return A data frame with genotype id and probability, ranked descending.
#' @rdname probbreed_bayesian
#' @export
pb_bayes_ranking <- function(probsup, what = c("performance", "stability")) {
  .need_probbreed("pb_bayes_ranking")
  what <- match.arg(what)
  tab <- if (what == "performance") probsup$across$perfo else probsup$across$stabil
  tab <- as.data.frame(tab)
  # standardise column names: id + prob
  names(tab)[1] <- "Genotype"
  pcol <- utils::tail(names(tab), 1)
  names(tab)[names(tab) == pcol] <- "Probability"
  tab <- tab[order(-tab$Probability), c("Genotype", "Probability")]
  rownames(tab) <- NULL
  tab
}

#' A polished probability-ranking bar plot
#'
#' Takes the tidy ranking from [pb_bayes_ranking()] and draws an attractive,
#' PbStatR-themed horizontal bar plot of the probability of superiority, with a
#' selection-intensity reference line.
#'
#' @param ranking A data frame from [pb_bayes_ranking()] (Genotype, Probability).
#' @param intensity Optional selection-intensity threshold to draw as a line.
#' @param top Show only the top `n` genotypes (NULL = all).
#' @return A `ggplot` object.
#' @rdname probbreed_bayesian
#' @export
pb_bayes_prob_bars <- function(ranking, intensity = NULL, top = NULL) {
  df <- ranking
  if (!is.null(top)) df <- utils::head(df, top)
  df$Genotype <- factor(df$Genotype, levels = rev(df$Genotype))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Probability,
                                        y = .data$Genotype,
                                        fill = .data$Probability)) +
    ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$Probability)),
                       hjust = -0.15, size = 3, color = "#2C3E50") +
    ggplot2::scale_fill_gradientn(colors = pb_palette("sunset", 8)) +
    ggplot2::scale_x_continuous(limits = c(0, 1),
                                expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(x = "Probability of superior performance", y = "Genotype",
                  title = "Bayesian probability of superiority",
                  subtitle = "Posterior probability each genotype is among the best") +
    pb_theme()
  if (!is.null(intensity))
    p <- p + ggplot2::geom_vline(xintercept = intensity, linetype = 2,
                                 color = "#FC4E07", linewidth = 0.8)
  p
}
