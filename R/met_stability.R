#' Multi-environment trial (MET) analysis
#'
#' Fits a mixed model across environments and returns BLUPs/means and a
#' combined ANOVA using the `metan` package.
#'
#' @param data A data frame in long format.
#' @param env,gen,rep Character names of environment, genotype and rep columns.
#' @param trait Character name of the response trait.
#' @return A `metan` waas/gamem object.
#' @export
pb_met <- function(data, env, gen, rep, trait) {
  metan::gamem_met(
    .data = data,
    env = !!rlang::sym(env),
    gen = !!rlang::sym(gen),
    rep = !!rlang::sym(rep),
    resp = !!rlang::sym(trait)
  )
}

#' Multi-trait selection index (Smith-Hazel / FAI-BLUP style)
#'
#' @param met_object Output of [pb_met()] with multiple traits.
#' @param ... Passed to [metan::fai_blup()].
#' @return A selection index object.
#' @export
pb_selection_index <- function(met_object, ...) {
  metan::fai_blup(met_object, ...)
}

#' Generic stability analysis dispatcher
#'
#' @param data Long-format MET data frame.
#' @param env,gen,rep,trait Column names.
#' @param method One of "ammi", "waasb", "eberhart".
#' @return The result of the chosen stability method.
#' @export
pb_stability <- function(data, env, gen, rep, trait,
                         method = c("ammi", "waasb", "eberhart")) {
  method <- match.arg(method)
  switch(method,
    ammi     = pb_ammi(data, env, gen, rep, trait),
    waasb    = pb_waasb(data, env, gen, rep, trait),
    eberhart = pb_eberhart_russell(data, env, gen, rep, trait)
  )
}

#' AMMI model (Additive Main effects and Multiplicative Interaction)
#' @inheritParams pb_stability
#' @return A `metan` AMMI model object (from `metan::performs_ammi`).
#' @export
pb_ammi <- function(data, env, gen, rep, trait) {
  metan::performs_ammi(
    .data = data,
    env = !!rlang::sym(env),
    gen = !!rlang::sym(gen),
    rep = !!rlang::sym(rep),
    resp = !!rlang::sym(trait)
  )
}

#' WAASB (Weighted Average of Absolute Scores from BLUP)
#' @inheritParams pb_stability
#' @return A `metan` WAASB object (from `metan::waasb`).
#' @export
pb_waasb <- function(data, env, gen, rep, trait) {
  metan::waasb(
    .data = data,
    env = !!rlang::sym(env),
    gen = !!rlang::sym(gen),
    rep = !!rlang::sym(rep),
    resp = !!rlang::sym(trait)
  )
}

#' Eberhart & Russell regression stability model
#' @inheritParams pb_stability
#' @return A `metan` regression object (from `metan::ge_reg`).
#' @export
pb_eberhart_russell <- function(data, env, gen, rep, trait) {
  metan::ge_reg(
    .data = data,
    env = !!rlang::sym(env),
    gen = !!rlang::sym(gen),
    rep = !!rlang::sym(rep),
    resp = !!rlang::sym(trait)
  )
}
