#' Post-hoc mean comparison tests
#'
#' Runs a fitted model through a chosen multiple-comparison procedure and
#' returns grouped means with compact letter display.
#'
#' @param data A data frame.
#' @param trait Response variable name.
#' @param factor Treatment factor name.
#' @param block Optional block factor name (for RCBD error control).
#' @param method One of "tukey", "duncan", "lsd", "snk" (Student-Newman-Keuls),
#'   "scheffe", "bonferroni".
#' @param alpha Significance level.
#' @return A list with the grouped means table (letters), the fitted model and
#'   the raw comparison object.
#' @export
#' @examples
#' df <- data.frame(
#'   trt = rep(paste0("T", 1:4), each = 4),
#'   blk = rep(1:4, times = 4),
#'   y = c(rnorm(4, 10), rnorm(4, 12), rnorm(4, 15), rnorm(4, 11))
#' )
#' pb_posthoc(df, "y", "trt", "blk", method = "tukey")
pb_posthoc <- function(data, trait, factor, block = NULL,
                       method = c("tukey", "duncan", "lsd", "snk",
                                  "scheffe", "bonferroni"),
                       alpha = 0.05) {
  method <- match.arg(method)
  data[[factor]] <- as.factor(data[[factor]])
  rhs <- factor
  if (!is.null(block)) {
    data[[block]] <- as.factor(data[[block]])
    rhs <- paste(factor, "+", block)
  }
  model <- stats::aov(stats::as.formula(paste(trait, "~", rhs)), data = data)

  cmp <- switch(method,
    tukey      = agricolae::HSD.test(model, factor, alpha = alpha, group = TRUE),
    duncan     = agricolae::duncan.test(model, factor, alpha = alpha, group = TRUE),
    lsd        = agricolae::LSD.test(model, factor, alpha = alpha, group = TRUE),
    snk        = agricolae::SNK.test(model, factor, alpha = alpha, group = TRUE),
    scheffe    = agricolae::scheffe.test(model, factor, alpha = alpha, group = TRUE),
    bonferroni = agricolae::LSD.test(model, factor, alpha = alpha,
                                     p.adj = "bonferroni", group = TRUE)
  )
  list(method = method, groups = cmp$groups, comparison = cmp, model = model)
}

#' Diagnostic and assumption tests for a linear model
#'
#' Normality (Shapiro-Wilk) and homogeneity of variance (Bartlett & Levene)
#' checks for the residuals of a treatment model.
#'
#' @param data A data frame.
#' @param trait Response variable name.
#' @param factor Treatment factor name.
#' @return A data frame of test statistics and p-values.
#' @export
pb_assumptions <- function(data, trait, factor) {
  data[[factor]] <- as.factor(data[[factor]])
  model <- stats::lm(stats::as.formula(paste(trait, "~", factor)), data = data)
  res <- stats::residuals(model)

  shap <- stats::shapiro.test(res)
  bart <- stats::bartlett.test(
    stats::as.formula(paste(trait, "~", factor)), data = data)
  # Levene via ANOVA on absolute deviations from group medians
  meds <- tapply(data[[trait]], data[[factor]], stats::median)
  absdev <- abs(data[[trait]] - meds[as.character(data[[factor]])])
  lev <- stats::anova(stats::lm(absdev ~ data[[factor]]))

  data.frame(
    Test = c("Shapiro-Wilk (normality)",
             "Bartlett (homogeneity)",
             "Levene (homogeneity)"),
    Statistic = c(shap$statistic, bart$statistic, lev[1, "F value"]),
    p_value = c(shap$p.value, bart$p.value, lev[1, "Pr(>F)"]),
    row.names = NULL
  )
}
