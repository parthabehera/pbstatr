#' PbStatR: Plant Breeding Statistical Analysis Toolkit
#'
#' A unified interface for genetic variability, experimental design, MET,
#' stability, biometrical genetics, molecular and Bayesian analyses.
#'
#' @keywords internal
"_PACKAGE"

# Defensive: silence any residual "no visible binding" NOTEs from NSE in
# ggplot2/dplyr. The code uses .data$ pronouns throughout; this is a safety net.
utils::globalVariables(c(".data", "Var1", "Var2", "value", "cycle", "meanG",
                         "fitted", "resid", "Trait", "Genotype", "z"))
