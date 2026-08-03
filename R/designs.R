#' Completely Randomised Design layout
#' @param treatments Character/numeric vector of treatment labels.
#' @param r Number of replications.
#' @param seed Optional RNG seed.
#' @return A data frame giving the field layout.
#' @export
design_crd <- function(treatments, r, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  agricolae::design.crd(trt = treatments, r = r, seed = seed %||% 0)$book
}

#' Randomised Complete Block Design layout
#' @inheritParams design_crd
#' @return A data frame containing the randomised field book (plot layout).
#' @export
design_rcbd <- function(treatments, r, seed = NULL) {
  agricolae::design.rcbd(trt = treatments, r = r, seed = seed %||% 0)$book
}

#' Latin Square Design layout
#' @param treatments Character/numeric vector of treatment labels.
#' @param seed Optional RNG seed.
#' @return A data frame containing the randomised field book (plot layout).
#' @export
design_lsd <- function(treatments, seed = NULL) {
  agricolae::design.lsd(trt = treatments, seed = seed %||% 0)$book
}

#' Factorial design layout (crossed factors in an RCBD/CRD)
#' @param factor_levels A named list of factor level vectors.
#' @param r Number of replications.
#' @param design Base design: "rcbd" or "crd".
#' @param seed Optional RNG seed.
#' @return A data frame containing the randomised field book (plot layout).
#' @export
design_factorial <- function(factor_levels, r, design = "rcbd", seed = NULL) {
  agricolae::design.ab(trt = lengths(factor_levels), r = r,
                       design = design, seed = seed %||% 0)$book
}

#' Split-plot design layout
#' @param main Vector of main-plot treatment labels.
#' @param sub Vector of sub-plot treatment labels.
#' @param r Number of replications.
#' @param seed Optional RNG seed.
#' @return A data frame containing the randomised field book (plot layout).
#' @export
design_split_plot <- function(main, sub, r, seed = NULL) {
  agricolae::design.split(trt1 = main, trt2 = sub, r = r,
                          design = "rcbd", seed = seed %||% 0)$book
}

#' Alpha-lattice (resolvable incomplete block) design layout
#' @param treatments Vector of treatment labels.
#' @param k Block size.
#' @param r Number of replications.
#' @param seed Optional RNG seed.
#' @return A data frame containing the randomised field book (plot layout).
#' @export
design_alpha_lattice <- function(treatments, k, r, seed = NULL) {
  agricolae::design.alpha(trt = treatments, k = k, r = r, seed = seed %||% 0)$book
}

#' Augmented (unreplicated test entries + replicated checks) design layout
#' @param checks Vector of check treatment labels.
#' @param new Vector of new/test treatment labels.
#' @param r Number of blocks.
#' @param seed Optional RNG seed.
#' @return A data frame containing the randomised field book (plot layout).
#' @export
design_augmented <- function(checks, new, r, seed = NULL) {
  agricolae::design.dau(trt1 = checks, trt2 = new, r = r, seed = seed %||% 0)$book
}

