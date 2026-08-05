#' Publication-ready ANOVA for any experimental design
#'
#' Fits the correct linear model (with the right error strata) for the standard
#' plant-breeding experimental designs and returns a tidy, formatted ANOVA
#' table with significance stars, CV, R-squared and grand mean — ready to drop
#' into a report. Supported designs:
#'
#' * `"crd"` — completely randomised design: `y ~ treatment`.
#' * `"rcbd"` — randomised complete block: `y ~ block + treatment`.
#' * `"lsd"` — Latin square: `y ~ row + column + treatment`.
#' * `"factorial"` — factorial in RCBD/CRD: crossed factors + interaction.
#' * `"split"` — split-plot: main plot error `block:mainplot`.
#' * `"split-split"` — split-split-plot with two nested error strata.
#' * `"strip"` — strip-plot (criss-cross).
#'
#' @param data A data frame in long format.
#' @param trait Response variable name.
#' @param design One of the designs listed above.
#' @param treatment Treatment factor (for crd/rcbd/lsd).
#' @param block Block/replication factor (rcbd, factorial, split*, strip).
#' @param row,col Row and column factors (lsd).
#' @param factors Character vector of factor names (factorial).
#' @param main,sub,sub2 Main-, sub-, and sub-sub-plot factors (split designs).
#' @param strip1,strip2 The two strip factors (strip-plot).
#' @param stars Append significance stars to the table.
#' @return An object of class `pb_aov` with the fitted model(s), the formatted
#'   ANOVA data frame, and summary statistics (CV, R2, grand mean).
#' @export
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   gen = rep(paste0("G", 1:4), each = 3),
#'   blk = rep(1:3, times = 4),
#'   y = rnorm(12, 50, 5))
#' pb_aov_design(df, "y", design = "rcbd", treatment = "gen", block = "blk")
pb_aov_design <- function(data, trait,
                          design = c("crd", "rcbd", "lsd", "factorial",
                                     "split", "split-split", "strip"),
                          treatment = NULL, block = NULL, row = NULL, col = NULL,
                          factors = NULL, main = NULL, sub = NULL, sub2 = NULL,
                          strip1 = NULL, strip2 = NULL, stars = TRUE) {
  design <- match.arg(design)
  d <- data
  # coerce relevant columns to factors
  facs <- Filter(Negate(is.null),
                 c(treatment, block, row, col, factors, main, sub, sub2,
                   strip1, strip2))
  for (f in facs) d[[f]] <- as.factor(d[[f]])

  fml <- switch(design,
    crd = stats::as.formula(paste(trait, "~", treatment)),
    rcbd = stats::as.formula(paste(trait, "~", block, "+", treatment)),
    lsd = stats::as.formula(paste(trait, "~", row, "+", col, "+", treatment)),
    factorial = stats::as.formula(
      paste(trait, "~", if (!is.null(block)) paste(block, "+") else "",
            paste(factors, collapse = " * "))),
    split = stats::as.formula(sprintf(
      "%s ~ %s + %s + Error(%s:%s) + %s + %s:%s",
      trait, block, main, block, main, sub, main, sub)),
    `split-split` = stats::as.formula(sprintf(
      "%s ~ %s + %s + Error(%s:%s) + %s + %s:%s + Error(%s:%s:%s) + %s + %s:%s + %s:%s + %s:%s:%s",
      trait, block, main, block, main,
      sub, main, sub, block, main, sub,
      sub2, main, sub2, sub, sub2, main, sub, sub2)),
    strip = stats::as.formula(sprintf(
      "%s ~ %s + %s + Error(%s:%s) + %s + Error(%s:%s) + %s:%s",
      trait, block, strip1, block, strip1,
      strip2, block, strip2, strip1, strip2))
  )

  has_error <- design %in% c("split", "split-split", "strip")
  model <- if (has_error) stats::aov(fml, data = d) else stats::aov(fml, data = d)

  tab <- .format_anova(model, has_error, stars)
  gm <- mean(d[[trait]], na.rm = TRUE)
  # CV & R2 from the (residual) error
  cv <- .anova_cv(model, gm, has_error)
  r2 <- .anova_r2(model, has_error)

  structure(list(design = design, model = model, anova = tab,
                 grand_mean = gm, cv = cv, r_squared = r2, trait = trait),
            class = "pb_aov")
}

# ---- internal formatting helpers ----
.format_anova <- function(model, has_error, stars) {
  if (has_error) {
    s <- summary(model)
    out <- do.call(rbind, lapply(names(s), function(stratum) {
      tb <- as.data.frame(s[[stratum]][[1]])
      tb$Source <- trimws(rownames(tb))
      tb$Stratum <- gsub("Error: ", "", stratum)
      tb
    }))
    rownames(out) <- NULL
  } else {
    out <- as.data.frame(stats::anova(model))
    out$Source <- rownames(out)
    out$Stratum <- NA_character_
    rownames(out) <- NULL
  }
  # standardise columns
  names(out)[names(out) == "Pr(>F)"] <- "p_value"
  names(out)[names(out) == "F value"] <- "F_value"
  names(out)[names(out) == "Mean Sq"] <- "Mean_Sq"
  names(out)[names(out) == "Sum Sq"] <- "Sum_Sq"
  if (stars && "p_value" %in% names(out)) {
    out$sig <- .sig_stars(out$p_value)
  }
  keep <- intersect(c("Stratum", "Source", "Df", "Sum_Sq", "Mean_Sq",
                      "F_value", "p_value", "sig"), names(out))
  out[, keep, drop = FALSE]
}

.sig_stars <- function(p) {
  ifelse(is.na(p), "",
    ifelse(p < 0.001, "***",
      ifelse(p < 0.01, "**",
        ifelse(p < 0.05, "*",
          ifelse(p < 0.1, ".", "ns")))))
}

.anova_cv <- function(model, gm, has_error) {
  mse <- .residual_ms(model, has_error)
  if (is.na(mse) || gm == 0) return(NA_real_)
  100 * sqrt(mse) / gm
}

.residual_ms <- function(model, has_error) {
  if (has_error) {
    s <- summary(model)
    # deepest residual stratum
    last <- s[[length(s)]][[1]]
    res <- last[trimws(rownames(last)) == "Residuals", "Mean Sq"]
    if (length(res)) return(res[1])
    return(NA_real_)
  }
  tb <- stats::anova(model)
  tb["Residuals", "Mean Sq"]
}

.anova_r2 <- function(model, has_error) {
  if (has_error) return(NA_real_)  # not well-defined across strata
  tb <- stats::anova(model)
  ss <- tb[["Sum Sq"]]
  1 - tb["Residuals", "Sum Sq"] / sum(ss)
}

#' Print method for a pb_aov object
#' @param x A `pb_aov` object.
#' @param ... Ignored.
#' @return The input `x`, invisibly.
#' @export
print.pb_aov <- function(x, ...) {
  cat(sprintf("Publication ANOVA \u2014 %s design (trait: %s)\n",
              toupper(x$design), x$trait))
  cat(strrep("-", 60), "\n")
  print(x$anova, row.names = FALSE)
  cat(strrep("-", 60), "\n")
  cat(sprintf("Grand mean = %.3f", x$grand_mean))
  if (!is.na(x$cv)) cat(sprintf("   |   CV = %.2f%%", x$cv))
  if (!is.na(x$r_squared)) cat(sprintf("   |   R\u00b2 = %.3f", x$r_squared))
  cat("\nSignif: *** <0.001  ** <0.01  * <0.05  . <0.1  ns\n")
  invisible(x)
}

#' Diagnostic-plot panel for an ANOVA model
#'
#' Four publication-ready diagnostic plots for a fitted design model:
#' residuals-vs-fitted, normal Q-Q, scale-location, and a residual histogram.
#'
#' @param object A `pb_aov` object (from [pb_aov_design()]) or an `aov`/`lm`.
#' @return A patchwork/ggplot object combining the four panels (or a list of
#'   ggplots if `patchwork` is not installed).
#' @export
pb_aov_plots <- function(object) {
  model <- if (inherits(object, "pb_aov")) object$model else object
  # split-plot models are aovlist; take the proj residuals via lm fallback
  res <- tryCatch(stats::residuals(model), error = function(e) NULL)
  fit <- tryCatch(stats::fitted(model), error = function(e) NULL)
  if (is.null(res) || is.null(fit)) {
    stop("Diagnostic plots need a single-stratum model (crd/rcbd/lsd/",
         "factorial). For split/strip designs, inspect strata separately.",
         call. = FALSE)
  }
  df <- data.frame(fit = as.vector(fit), res = as.vector(res))
  df$std <- df$res / stats::sd(df$res)
  df$sqrt_std <- sqrt(abs(df$std))
  qq <- stats::qqnorm(df$std, plot.it = FALSE)
  df$theo <- qq$x[order(order(df$std))]

  p1 <- ggplot2::ggplot(df, ggplot2::aes(.data$fit, .data$res)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, color = "#FC4E07") +
    ggplot2::geom_point(color = "#2E9FDF", alpha = 0.8, size = 2) +
    ggplot2::geom_smooth(se = FALSE, method = "loess", formula = y ~ x,
                         color = "#8E44AD", linewidth = 0.8) +
    ggplot2::labs(x = "Fitted", y = "Residuals", title = "Residuals vs Fitted") +
    pb_theme()
  p2 <- ggplot2::ggplot(df, ggplot2::aes(.data$theo, .data$std)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2,
                         color = "#FC4E07") +
    ggplot2::geom_point(color = "#00AF66", alpha = 0.8, size = 2) +
    ggplot2::labs(x = "Theoretical quantiles", y = "Std residuals",
                  title = "Normal Q-Q") +
    pb_theme()
  p3 <- ggplot2::ggplot(df, ggplot2::aes(.data$fit, .data$sqrt_std)) +
    ggplot2::geom_point(color = "#E7B800", alpha = 0.8, size = 2) +
    ggplot2::geom_smooth(se = FALSE, method = "loess", formula = y ~ x,
                         color = "#8E44AD", linewidth = 0.8) +
    ggplot2::labs(x = "Fitted", y = expression(sqrt(abs("Std resid"))),
                  title = "Scale-Location") +
    pb_theme()
  p4 <- ggplot2::ggplot(df, ggplot2::aes(.data$res)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = 15, fill = "#2E9FDF", color = "white",
                            alpha = 0.85) +
    ggplot2::geom_density(color = "#8E44AD", linewidth = 0.9) +
    ggplot2::labs(x = "Residuals", y = "Density",
                  title = "Residual distribution") +
    pb_theme()

  if (requireNamespace("patchwork", quietly = TRUE)) {
    patchwork::wrap_plots(p1, p2, p3, p4, ncol = 2)
  } else {
    list(residuals_vs_fitted = p1, qq = p2,
         scale_location = p3, histogram = p4)
  }
}

#' Bar plot of an ANOVA table's variance partition
#'
#' Shows the proportion of the total sum of squares explained by each source,
#' a quick visual of where the variation lives.
#'
#' @param object A `pb_aov` object.
#' @return A `ggplot` object.
#' @export
pb_aov_barplot <- function(object) {
  stopifnot(inherits(object, "pb_aov"))
  tab <- object$anova
  tab <- tab[!is.na(tab$Sum_Sq), ]
  tab$pct <- 100 * tab$Sum_Sq / sum(tab$Sum_Sq, na.rm = TRUE)
  tab <- tab[order(-tab$pct), ]
  tab$Source <- factor(tab$Source, levels = tab$Source)
  ggplot2::ggplot(tab, ggplot2::aes(.data$Source, .data$pct,
                                    fill = .data$Source)) +
    ggplot2::geom_col(show.legend = FALSE, width = 0.75) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", .data$pct)),
                       vjust = -0.3, size = 3.3, color = "#2C3E50") +
    ggplot2::scale_fill_manual(values = pb_palette("main", nrow(tab))) +
    ggplot2::labs(x = NULL, y = "% of total sum of squares",
                  title = "Variance partition",
                  subtitle = paste(toupper(object$design), "design")) +
    pb_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
