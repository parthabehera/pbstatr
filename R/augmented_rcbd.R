#' Augmented RCBD analysis (augmentedRCBD)
#'
#' Full analysis of an augmented randomised complete block design using the
#' `augmentedRCBD` package: ANOVA, adjusted means for the (unreplicated) test
#' treatments, standard errors of comparisons, CV and descriptive statistics.
#'
#' @param block Factor/vector of block assignments.
#' @param treatment Factor/vector of treatment labels (checks are replicated).
#' @param y Numeric response vector.
#' @param checks Character vector naming the check treatments. If NULL, checks
#'   are inferred as those appearing in more than one block.
#' @param method_comparison Multiple-comparison method for adjusted means.
#' @param alpha Significance level.
#' @return An `augmentedRCBD` object (ANOVA, adjusted means, SEs, CV, etc.).
#' @export
pb_augmented <- function(block, treatment, y, checks = NULL,
                         method_comparison = "lsd", alpha = 0.05) {
  if (!requireNamespace("augmentedRCBD", quietly = TRUE))
    stop("Install the 'augmentedRCBD' package to run pb_augmented().",
         call. = FALSE)
  block <- as.factor(block)
  treatment <- as.factor(treatment)
  if (is.null(checks)) {
    tab <- table(treatment, block)
    reps <- rowSums(tab > 0)
    checks <- names(reps)[reps > 1]
  }
  augmentedRCBD::augmentedRCBD(
    block = block, treatment = treatment, y = y,
    checks = checks, method.comp = method_comparison,
    alpha = alpha, group = TRUE, console = FALSE
  )
}

#' Augmented RCBD across multiple traits / locations
#'
#' Applies [pb_augmented()] to several trait columns of a data frame and
#' returns the list of analyses plus a combined adjusted-means table.
#'
#' @param data A data frame.
#' @param block,treatment Column names for block and treatment.
#' @param traits Character vector of trait columns.
#' @param checks Optional check names (see [pb_augmented()]).
#' @return A list with per-trait analyses and a merged adjusted-means table.
#' @export
pb_augmented_multi <- function(data, block, treatment, traits, checks = NULL) {
  .check_columns(data, c(block, treatment, traits))
  analyses <- lapply(traits, function(tr) {
    pb_augmented(data[[block]], data[[treatment]], data[[tr]], checks = checks)
  })
  names(analyses) <- traits

  means <- lapply(traits, function(tr) {
    am <- analyses[[tr]]$Means
    data.frame(Treatment = am$Treatment, setNames(list(am$`Adjusted Means`), tr),
               check.names = FALSE)
  })
  merged <- Reduce(function(a, b) merge(a, b, by = "Treatment", all = TRUE), means)

  list(analyses = analyses, adjusted_means = merged)
}
