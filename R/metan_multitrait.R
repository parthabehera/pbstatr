#' Multi-trait selection and metan visualisations
#'
#' A cohesive set of wrappers around the multi-trait selection indices and the
#' rich plotting functions of the `metan` package (Olivoto & Lucio, 2020).
#' Fit a genotype model once with [pb_met()] (which calls `metan::gamem_met`)
#' or [pb_waasb()], then feed it to the index and plot helpers below.
#'
#' All functions require the `metan` package (a Suggests dependency) and error
#' with an informative message if it is not installed.
#'
#' @name metan_multitrait
NULL

# internal guard
.need_metan <- function(fn) {
  if (!requireNamespace("metan", quietly = TRUE))
    stop("`", fn, "()` needs the 'metan' package. ",
         "Install it with install.packages('metan').", call. = FALSE)
}

#' MGIDI - Multi-trait Genotype-Ideotype Distance Index
#'
#' Computes the MGIDI index (Olivoto & Nardino, 2020) from a fitted genotype
#' model, ranking genotypes by their distance to an ideotype across all traits.
#'
#' @param model A model from [pb_met()] / `metan::gamem_met` / `metan::gamem`,
#'   or a two-way table of BLUPs (genotypes in rows, traits in columns).
#' @param SI Selection intensity, percent of genotypes to select (0-100).
#' @param weights Optional numeric vector of trait weights (length = n traits).
#' @param ideotype Optional character vector ("h"/"l" per trait) for the desired
#'   direction of each trait (higher/lower is better).
#' @param verbose Print a console summary.
#' @return A `metan` `mgidi` object (selected genotypes, index values,
#'   factor contributions, selection gains).
#' @rdname metan_multitrait
#' @export
pb_mgidi <- function(model, SI = 15, weights = NULL, ideotype = NULL,
                     verbose = FALSE) {
  .need_metan("pb_mgidi")
  metan::mgidi(model, SI = SI, weights = weights, ideotype = ideotype,
               verbose = verbose)
}

#' MTSI - Multi-Trait Stability Index
#'
#' Computes the MTSI (Olivoto et al., 2019), which selects genotypes on both
#' mean performance and stability simultaneously from a `waasb`-type model.
#'
#' @param model A model from [pb_waasb()] (class `waasb`) with multiple traits.
#' @param SI Selection intensity (percent, 0-100).
#' @param verbose Print a console summary.
#' @return A `metan` `mtsi` object.
#' @rdname metan_multitrait
#' @export
pb_mtsi <- function(model, SI = 15, verbose = FALSE) {
  .need_metan("pb_mtsi")
  metan::mtsi(model, SI = SI, verbose = verbose)
}

#' FAI-BLUP selection index
#'
#' The FAI-BLUP multi-trait index (Rocha et al., 2018), based on factor analysis
#' and ideotype design, computed from a fitted genotype model.
#'
#' @param model A model from [pb_met()] / `metan::gamem_met`.
#' @param DI,UI Desired and undesired ideotype specifications passed to
#'   `metan::fai_blup` (optional).
#' @param SI Selection intensity (percent).
#' @param verbose Print a console summary.
#' @return A `metan` `fai_blup` object.
#' @rdname metan_multitrait
#' @export
pb_fai_blup <- function(model, DI = NULL, UI = NULL, SI = 15, verbose = FALSE) {
  .need_metan("pb_fai_blup")
  args <- list(.data = model, SI = SI, verbose = verbose)
  if (!is.null(DI)) args$DI <- DI
  if (!is.null(UI)) args$UI <- UI
  do.call(metan::fai_blup, args)
}

#' Smith-Hazel selection index
#'
#' The classical Smith-Hazel index, combining trait BLUPs with economic weights
#' and the genotypic/phenotypic (co)variance structure.
#'
#' @param model A model from [pb_met()] / `metan::gamem_met`.
#' @param weights Optional named numeric vector of economic weights per trait.
#' @param SI Selection intensity (percent).
#' @return A `metan` `sh` (Smith-Hazel) object.
#' @rdname metan_multitrait
#' @export
pb_smith_hazel <- function(model, weights = NULL, SI = 15) {
  .need_metan("pb_smith_hazel")
  args <- list(model, SI = SI)
  if (!is.null(weights)) args$weights <- weights
  do.call(metan::Smith_Hazel, args)
}

# ---------------------------------------------------------------------------
# Plotting helpers
# ---------------------------------------------------------------------------

#' Radar / contribution plot for MGIDI or MTSI
#'
#' Produces the factor-contribution radar plot (the default `metan` view) that
#' shows the strengths and weaknesses of the selected genotypes.
#'
#' @param index_obj An object from [pb_mgidi()] or [pb_mtsi()].
#' @param type Plot type: "contribution" (radar, default) or "index".
#' @param ... Further arguments passed to `metan`'s `plot()` method.
#' @return A `ggplot` object.
#' @rdname metan_multitrait
#' @export
pb_radar_plot <- function(index_obj, type = "contribution", ...) {
  .need_metan("pb_radar_plot")
  plot(index_obj, type = type, ...)
}

#' Venn diagram of genotypes selected by different indices
#'
#' Compares the sets of genotypes selected by two or more multi-trait indices
#' (e.g. MGIDI vs FAI-BLUP vs Smith-Hazel) as a Venn diagram.
#'
#' @param ... Two to four named character vectors of selected genotype ids,
#'   or index objects from which selections are extracted automatically.
#' @param names Optional labels for the sets.
#' @return A `ggplot`/grid Venn diagram.
#' @rdname metan_multitrait
#' @export
pb_venn_plot <- function(..., names = NULL) {
  .need_metan("pb_venn_plot")
  sets <- list(...)
  # extract $sel_gen from index objects, else assume character vectors
  sets <- lapply(sets, function(s) {
    if (is.character(s)) s
    else if (!is.null(s$sel_gen)) s$sel_gen
    else if (!is.null(s$sel_dif) && !is.null(s$sel_dif$GEN)) unique(s$sel_dif$GEN)
    else stop("Each argument must be a character vector or an index object ",
              "with a `$sel_gen` element.", call. = FALSE)
  })
  if (!is.null(names)) names(sets) <- names
  do.call(metan::venn_plot, sets)
}

#' WAASB vs WAASBY scatter plot (plot_scores type 3)
#'
#' The classic biplot of the weighted-average stability (WAASB) against mean
#' performance, dividing genotypes into the four productivity/stability
#' quadrants.
#'
#' @param waasb_obj An object from [pb_waasb()].
#' @param type `metan::plot_scores` type (default 3 = WAASB x response).
#' @param ... Passed to `metan::plot_scores`.
#' @return A `ggplot` object.
#' @rdname metan_multitrait
#' @export
pb_waasb_xy_plot <- function(waasb_obj, type = 3, ...) {
  .need_metan("pb_waasb_xy_plot")
  metan::plot_scores(waasb_obj, type = type, ...)
}

#' WAASBY superiority index bar plot
#'
#' Ranks genotypes by the WAASBY index (a weighted blend of stability and mean
#' performance), where higher is better.
#'
#' @param waasb_obj An object from [pb_waasb()].
#' @param ... Passed to `metan::plot_waasby`.
#' @return A `ggplot` object.
#' @rdname metan_multitrait
#' @export
pb_waasby_plot <- function(waasb_obj, ...) {
  .need_metan("pb_waasby_plot")
  metan::plot_waasby(waasb_obj, ...)
}

#' BLUP plot for genotype effects
#'
#' Plots the genotype BLUPs (with confidence intervals) from a fitted mixed
#' model, ordered by predicted merit.
#'
#' @param model A model from [pb_met()] / `metan::gamem_met` / [pb_waasb()].
#' @param ... Passed to `metan::plot_blup`.
#' @return A `ggplot` object.
#' @rdname metan_multitrait
#' @export
pb_blup_plot <- function(model, ...) {
  .need_metan("pb_blup_plot")
  metan::plot_blup(model, ...)
}

#' Extract a results table from any metan object
#'
#' A thin wrapper around `metan::get_model_data()` (a.k.a. `gmd`) to pull tidy
#' result tables (e.g. "MGIDI", "WAASB", "WAASBY", "blupg", "genpar") out of the
#' fitted objects.
#'
#' @param object Any fitted `metan` object (model or index).
#' @param what The statistic to extract (see `?metan::get_model_data`).
#' @return A data frame / tibble of the requested results.
#' @rdname metan_multitrait
#' @export
pb_get_results <- function(object, what = "MGIDI") {
  .need_metan("pb_get_results")
  metan::get_model_data(object, what = what)
}
