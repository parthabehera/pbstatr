#' One-call analysis of a variety trial (student-friendly)
#'
#' A single function that walks a beginner through a complete single-trait
#' analysis: it runs the ANOVA, checks assumptions, estimates genetic
#' parameters, runs a post-hoc test, and prints a plain-language summary.
#' Everything it computes is also returned invisibly for further use.
#'
#' @param data A data frame.
#' @param trait Response variable name (e.g. "yield").
#' @param genotype Genotype / treatment column name.
#' @param block Block / replication column name (RCBD assumed).
#' @param posthoc Post-hoc method for mean grouping (see [pb_posthoc()]).
#' @param verbose Print the guided summary to the console.
#' @return Invisibly, a list with `anova`, `assumptions`, `genetic_params`,
#'   `means` (grouped) and `plots`.
#' @export
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   variety = rep(paste0("V", 1:6), each = 3),
#'   block   = rep(1:3, times = 6),
#'   yield   = rnorm(18, 40, 6)
#' )
#' pb_analyze(df, "yield", "variety", "block")
pb_analyze <- function(data, trait, genotype, block,
                       posthoc = "tukey", verbose = TRUE) {
  .check_columns(data, c(trait, genotype, block))

  fit  <- pb_anova(data, trait, genotype, block, design = "rcbd")
  assum <- pb_assumptions(data, trait, genotype)
  gp    <- pb_genetic_params(data, trait, genotype, block)
  ph    <- pb_posthoc(data, trait, genotype, block, method = posthoc)

  p_val <- fit$anova[genotype, "Pr(>F)"]
  sig   <- if (!is.na(p_val) && p_val < 0.05) "significant" else "not significant"

  plots <- list(
    boxplot   = .plot_box(data, trait, genotype),
    means_bar = .plot_means(ph$groups, trait),
    diagnostics = .plot_diag(fit$model)
  )

  if (verbose) {
    cat("\n=== PbStatR guided analysis ===\n")
    cat(sprintf("Trait: %s   |   Genotypes: %d   |   Reps: %d\n",
                trait, length(unique(data[[genotype]])), fit$replications))
    cat(sprintf("\n1. ANOVA: genotype effect is %s (p = %.4g).\n", sig, p_val))
    cat(sprintf("2. Heritability (broad sense) = %.2f  ->  %s.\n",
                gp$Heritability, .h2_words(gp$Heritability)))
    cat(sprintf("   GCV = %.1f%%, PCV = %.1f%%, GAM = %.1f%%.\n",
                gp$GCV, gp$PCV, gp$GAM))
    cat(sprintf("3. Assumptions (p>0.05 is good): normality p = %.3f, ",
                assum$p_value[1]))
    cat(sprintf("equal variance p = %.3f.\n", assum$p_value[2]))
    cat(sprintf("4. Best entries (%s grouping): see $means.\n", posthoc))
    cat("\nTip: view plots with result$plots$boxplot, $means_bar, $diagnostics.\n")
    cat("================================\n\n")
  }

  invisible(list(anova = fit$anova, assumptions = assum,
                 genetic_params = gp, means = ph$groups, plots = plots))
}

#' Quick trait summary table (student-friendly)
#'
#' Descriptive statistics per genotype (mean, sd, cv, min, max, n) for one or
#' more traits — a fast first look at the data.
#'
#' @param data A data frame.
#' @param traits Character vector of trait column(s).
#' @param genotype Genotype column name.
#' @return A tidy data frame of summaries.
#' @export
pb_summary <- function(data, traits, genotype) {
  .check_columns(data, c(traits, genotype))
  out <- lapply(traits, function(tr) {
    ag <- stats::aggregate(data[[tr]], list(Genotype = data[[genotype]]),
      function(x) c(mean = mean(x, na.rm = TRUE), sd = stats::sd(x, na.rm = TRUE),
                    min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE),
                    n = sum(!is.na(x))))
    res <- data.frame(Trait = tr, Genotype = ag$Genotype, ag$x)
    res$cv <- 100 * res$sd / res$mean
    res
  })
  do.call(rbind, out)
}

#' List everything PbStatR can do (student-friendly menu)
#'
#' Prints a categorised list of the package's functions with a one-line
#' description of each, so newcomers can discover the toolkit.
#' @return Invisibly, a data frame of functions and descriptions.
#' @export
pb_help <- function() {
  menu <- rbind(
    c("Genetic variability", "pb_anova / pb_genetic_params", "ANOVA, GCV, PCV, h2, GA, GAM"),
    c("Guided analysis", "pb_analyze / pb_summary / pb_help", "one-call trial analysis & summaries"),
    c("Post-hoc tests", "pb_posthoc / pb_assumptions", "Tukey, Duncan, LSD; normality & variance"),
    c("Associations", "pb_correlation / pb_path_analysis", "correlations & path coefficients"),
    c("Diversity", "pb_diversity / pb_diversity_indices", "clustering, Shannon/Simpson"),
    c("Multivariate", "pb_pca / pb_hcpc / pb_heatmap", "PCA, HCPC, heatmaps"),
    c("Field designs", "design_* / field_*", "agricolae + FielDHub layouts & maps"),
    c("MET & stability", "pb_met / pb_ammi / pb_waasb", "multi-environment, AMMI, WAASB"),
    c("Biometrics", "pb_griffing / pb_generation_mean", "diallel & generation mean"),
    c("Genomic selection", "pb_blup / pb_grm / pb_gblup", "BLUP, GRM, GBLUP + CV"),
    c("Simulation", "pb_sim_founders / pb_sim_pipeline", "AlphaSimR breeding schemes")
  )
  df <- as.data.frame(menu, stringsAsFactors = FALSE)
  names(df) <- c("Area", "Functions", "What_it_does")
  cat("\nPbStatR toolkit — type ?function for help on any entry:\n\n")
  print(df, row.names = FALSE, right = FALSE)
  cat("\nStart with:  pb_analyze(data, trait, genotype, block)\n\n")
  invisible(df)
}

# ---- internal plotting & validation helpers ----
.check_columns <- function(data, cols) {
  miss <- setdiff(cols, names(data))
  if (length(miss))
    stop("These columns are not in your data: ",
         paste(miss, collapse = ", "),
         ".\nAvailable columns: ", paste(names(data), collapse = ", "),
         call. = FALSE)
}

.h2_words <- function(h2) {
  if (is.na(h2)) return("unavailable")
  if (h2 >= 0.6) "high (selection should be effective)"
  else if (h2 >= 0.3) "moderate"
  else "low (environment dominates; select with care)"
}

.plot_box <- function(data, trait, genotype) {
  n <- length(unique(data[[genotype]]))
  ggplot2::ggplot(data, ggplot2::aes(x = stats::reorder(.data[[genotype]],
                    .data[[trait]], FUN = stats::median),
                    y = .data[[trait]], fill = .data[[genotype]])) +
    ggplot2::geom_boxplot(show.legend = FALSE, alpha = 0.85,
                          outlier.color = "#34495E", color = "#2C3E50") +
    ggplot2::scale_fill_manual(values = pb_palette("main", n)) +
    ggplot2::labs(x = genotype, y = trait,
                  title = paste(trait, "distribution by", genotype),
                  subtitle = "Boxes ordered by median") +
    pb_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

.plot_means <- function(groups, trait) {
  g <- groups
  g$Genotype <- rownames(g)
  yval <- names(g)[1]
  ggplot2::ggplot(g, ggplot2::aes(x = stats::reorder(.data[["Genotype"]],
                    .data[[yval]]), y = .data[[yval]],
                    fill = .data[[yval]])) +
    ggplot2::geom_col(show.legend = FALSE, width = 0.75) +
    ggplot2::geom_text(ggplot2::aes(label = .data[["groups"]]),
                       vjust = -0.4, size = 3.5, color = "#2C3E50",
                       fontface = "bold") +
    ggplot2::scale_fill_gradientn(colors = pb_palette("cool", 8)) +
    ggplot2::labs(x = "Genotype", y = paste("Mean", trait),
                  title = "Ranked means with grouping letters",
                  subtitle = "Bars sharing a letter are not significantly different") +
    pb_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

.plot_diag <- function(model) {
  df <- data.frame(fitted = stats::fitted(model),
                   resid = stats::residuals(model))
  ggplot2::ggplot(df, ggplot2::aes(.data$fitted, .data$resid)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, color = "#FC4E07",
                        linewidth = 0.7) +
    ggplot2::geom_point(alpha = 0.75, size = 2.4, color = "#2E9FDF") +
    ggplot2::geom_smooth(se = FALSE, method = "loess", formula = y ~ x,
                         color = "#8E44AD", linewidth = 0.9) +
    ggplot2::labs(x = "Fitted values", y = "Residuals",
                  title = "Residuals vs fitted",
                  subtitle = "A flat, even cloud around zero means the model fits well") +
    pb_theme()
}
