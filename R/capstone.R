#' Student-research capstone: guided exploration and dashboards
#'
#' A small set of high-level, beginner-friendly entry points that tie the whole
#' package together. `pb_explore()` recommends the right function for a
#' question; `pb_genetic_dashboard()` renders one attractive multi-panel figure
#' summarising the genetic analysis of a trial; and `pb_workflow()` prints a
#' copy-paste analysis recipe for common study types.
#'
#' @name capstone
NULL

#' Guided explorer: which function do I need?
#'
#' Answers "what should I run for X?" by mapping a research goal to the relevant
#' PbStatR functions, with a one-line description and a ready example call.
#' Call with no argument to see all goals.
#'
#' @param goal A short keyword describing the goal, e.g. "heritability",
#'   "stability", "gwas", "diversity", "selection", "design", "correlation".
#'   Partial matches work. `NULL` lists every goal.
#' @return Invisibly, a data frame of matching goals and functions; also printed.
#' @export
#' @examples
#' pb_explore("heritability")
#' pb_explore("stability")
#' pb_explore()          # list everything
pb_explore <- function(goal = NULL) {
  map <- rbind(
    c("variability", "Genetic variability (GCV, PCV, h2, GAM)",
      "pb_genetic_params(data, trait, gen, block)"),
    c("heritability", "Broad/narrow-sense heritability",
      "pb_heritability(data, trait, gen, block)"),
    c("variance components", "Partition Vg, Ve, Vp",
      "pb_varcomp(data, trait, gen, block)"),
    c("genetic advance", "Expected genetic gain (GA, GAM)",
      "pb_genetic_advance(data, trait, gen, block)"),
    c("selection response", "Breeder's equation R = h2 * S",
      "pb_breeders_eqn(h2 = 0.4, S = 5)"),
    c("anova", "Publication ANOVA for any design",
      "pb_aov_design(data, trait, design = 'rcbd', treatment = 'gen', block = 'blk')"),
    c("posthoc", "Mean comparison (Tukey/Duncan/LSD)",
      "pb_posthoc(data, trait, gen, block, method = 'tukey')"),
    c("correlation", "Trait correlations + heatmap",
      "pb_correlation(data, traits); pb_heatmap(data, traits)"),
    c("path analysis", "Direct/indirect effects",
      "pb_path_analysis(data, dependent, causal)"),
    c("diversity", "Clustering & diversity",
      "pb_diversity(data, traits, id)"),
    c("multivariate", "PCA / HCPC",
      "pb_pca(data, traits, groups)"),
    c("stability", "GxE stability (all methods)",
      "pb_gxe(data, gen, env, rep, trait)"),
    c("stability ranking", "Combined stability table + plot",
      "pb_stability_ranks(data, gen, env, rep, trait)"),
    c("ammi waasb", "AMMI / WAASB models",
      "pb_ammi(...); pb_waasb(...)"),
    c("multi-trait selection", "MGIDI / FAI-BLUP / Smith-Hazel",
      "pb_mgidi(model); pb_fai_blup(model)"),
    c("design", "Lay out a field experiment",
      "field_rcbd(t = 10, reps = 3); design_alpha_lattice(...)"),
    c("augmented", "Augmented design analysis",
      "pb_aug_analyze(data, block, genotype, trait)"),
    c("biometrics", "Line x tester / diallel / generation mean",
      "pb_griffing(...); pb_line_tester(...)"),
    c("blup", "Mixed-model BLUPs",
      "pb_blup(data, trait, gen)"),
    c("genomic selection", "GBLUP / GRM / cross-validation",
      "pb_gblup(y, M, cv_folds = 5)"),
    c("machine learning", "ML genomic prediction",
      "pb_ml_compare(y, M)"),
    c("gwas", "Genome-wide association + Manhattan",
      "pb_gwas_rmvp(...); pb_manhattan(map, pvalues)"),
    c("bayesian", "Bayesian probability of superiority",
      "pb_bayes_met(...); pb_bayes_prob_bars(ranking)"),
    c("simulation", "Breeding-scheme simulation",
      "pb_sim_founders(); pb_sim_pipeline(sim)"),
    c("report", "One-click HTML/Word report",
      "pb_report(data, trait, gen, block, env)")
  )
  df <- data.frame(Goal = map[, 1], Does = map[, 2], Example = map[, 3],
                   stringsAsFactors = FALSE)
  if (!is.null(goal)) {
    hit <- grepl(goal, df$Goal, ignore.case = TRUE) |
           grepl(goal, df$Does, ignore.case = TRUE)
    df <- df[hit, , drop = FALSE]
    if (nrow(df) == 0) {
      cat("No match for '", goal, "'. Try pb_explore() to see all goals.\n",
          sep = "")
      return(invisible(df))
    }
  }
  cat("\nPbStatR \u2014 what to run:\n\n")
  for (i in seq_len(nrow(df))) {
    cat(sprintf("  \u2022 %s\n      %s\n      > %s\n\n",
                df$Does[i], paste0("goal: ", df$Goal[i]), df$Example[i]))
  }
  invisible(df)
}

#' Print a copy-paste analysis recipe for a study type
#'
#' @param study One of "variety_trial", "met" (multi-environment),
#'   "augmented", "gwas", "genomic_selection".
#' @return Invisibly, the recipe lines; also printed.
#' @export
pb_workflow <- function(study = c("variety_trial", "met", "augmented",
                                  "gwas", "genomic_selection")) {
  study <- match.arg(study)
  recipes <- list(
    variety_trial = c(
      "# Single-location variety trial (RCBD)",
      "res <- pb_analyze(data, trait, genotype = 'gen', block = 'blk')",
      "pb_aov_design(data, trait, design = 'rcbd', treatment = 'gen', block = 'blk')",
      "pb_genetic_params(data, trait, 'gen', 'blk')",
      "pb_posthoc(data, trait, 'gen', 'blk', method = 'tukey')",
      "pb_report(data, trait, 'gen', 'blk')"),
    met = c(
      "# Multi-environment trial",
      "pb_gxe(data, gen = 'gen', env = 'env', rep = 'rep', trait = 'y')",
      "ranks <- pb_stability_ranks(data, 'gen', 'env', 'rep', 'y')",
      "pb_plot_stability(ranks)",
      "pb_gxe_heatmap(data, 'gen', 'env', 'y', scale = 'genotype')",
      "# multi-trait selection:",
      "mod <- pb_met(data, 'env', 'gen', 'rep', 'y'); pb_mgidi(mod)"),
    augmented = c(
      "# Augmented design (unreplicated tests + replicated checks)",
      "pb_aug_analyze(data, block = 'blk', genotype = 'gen', trait = 'y')"),
    gwas = c(
      "# GWAS",
      "map  <- pb_marker_map(mapdata)",
      "res  <- pb_gwas_rmvp(phe, geno, map, methods = c('MLM','FarmCPU'))",
      "pb_manhattan(map, pvalues); pb_qqplot(pvalues)"),
    genomic_selection = c(
      "# Genomic selection",
      "G  <- pb_grm(M)",
      "gs <- pb_gblup(y, M, cv_folds = 5); gs$cv_accuracy",
      "pb_ml_compare(y, M)   # benchmark ML vs GBLUP",
      "top <- pb_select(setNames(gs$GEBV, ids), proportion = 0.1)")
  )
  lines <- recipes[[study]]
  cat("\n", paste(lines, collapse = "\n"), "\n\n", sep = "")
  invisible(lines)
}

#' Genetic-parameters dashboard (one attractive figure)
#'
#' Renders a single multi-panel figure summarising the genetic analysis of a
#' trial: (a) genotype means with error bars, (b) a variance-component donut,
#' (c) a GCV/PCV/heritability/GAM parameter bar, and (d) a heritability gauge.
#' Designed so a student can paste one call and get a complete visual summary.
#'
#' @param data A data frame (RCBD).
#' @param trait Response trait column name.
#' @param genotype Genotype factor column name.
#' @param block Block factor column name.
#' @return A patchwork/ggplot object (or a list of ggplots if patchwork is
#'   not installed).
#' @export
#' @examples
#' set.seed(1)
#' df <- data.frame(gen = rep(paste0("G", 1:8), each = 3),
#'                  blk = rep(1:3, times = 8), y = rnorm(24, 50, 6))
#' pb_genetic_dashboard(df, "y", "gen", "blk")
pb_genetic_dashboard <- function(data, trait, genotype, block) {
  gp <- pb_genetic_params(data, trait, genotype, block)
  vc <- pb_varcomp(data, trait, genotype, block)

  # (a) genotype means +/- SE
  ag <- stats::aggregate(data[[trait]], list(g = data[[genotype]]),
                         function(x) c(m = mean(x), se = stats::sd(x) /
                                         sqrt(length(x))))
  means <- data.frame(Genotype = ag$g, Mean = ag$x[, "m"], SE = ag$x[, "se"])
  means <- means[order(-means$Mean), ]
  means$Genotype <- factor(means$Genotype, levels = means$Genotype)
  p_means <- ggplot2::ggplot(means, ggplot2::aes(.data$Genotype, .data$Mean,
                                                 fill = .data$Mean)) +
    ggplot2::geom_col(show.legend = FALSE, width = 0.75) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$Mean - .data$SE,
                                        ymax = .data$Mean + .data$SE),
                           width = 0.25, color = "#2C3E50") +
    ggplot2::scale_fill_gradientn(colors = pb_palette("cool", 8)) +
    ggplot2::labs(x = "Genotype", y = paste("Mean", trait),
                  title = "Genotype means \u00b1 SE") +
    pb_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # (b) variance-component donut
  don <- vc[vc$Component != "Phenotypic (Vp)", ]
  don$frac <- don$Variance / sum(don$Variance)
  don$ymax <- cumsum(don$frac); don$ymin <- c(0, utils::head(don$ymax, -1))
  p_var <- ggplot2::ggplot(don, ggplot2::aes(ymax = .data$ymax,
                ymin = .data$ymin, xmax = 4, xmin = 3,
                fill = .data$Component)) +
    ggplot2::geom_rect() +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::xlim(c(1, 4)) +
    ggplot2::scale_fill_manual(values = c("#2E9FDF", "#FC4E07")) +
    ggplot2::annotate("text", x = 1, y = 0,
                      label = sprintf("h\u00b2\n%.2f", gp$Heritability),
                      size = 5, fontface = "bold", color = "#2C3E50") +
    ggplot2::labs(title = "Variance partition", fill = NULL) +
    pb_theme() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   legend.position = "bottom")

  # (c) parameter bars
  par_df <- data.frame(
    Parameter = factor(c("GCV", "PCV", "ECV", "GAM"),
                       levels = c("GCV", "PCV", "ECV", "GAM")),
    Value = c(gp$GCV, gp$PCV, gp$ECV, gp$GAM))
  p_par <- ggplot2::ggplot(par_df, ggplot2::aes(.data$Parameter, .data$Value,
                                                fill = .data$Parameter)) +
    ggplot2::geom_col(show.legend = FALSE, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", .data$Value)),
                       vjust = -0.3, size = 3.3, color = "#2C3E50",
                       fontface = "bold") +
    ggplot2::scale_fill_manual(values = pb_palette("main", 4)) +
    ggplot2::labs(x = NULL, y = "Percent",
                  title = "Genetic parameters") +
    pb_theme()

  # (d) heritability gauge (semicircle)
  h2 <- min(max(gp$Heritability, 0), 1)
  gauge <- data.frame(
    ymax = c(h2, 1), ymin = c(0, h2),
    part = c("h2", "rest"))
  p_h2 <- ggplot2::ggplot(gauge, ggplot2::aes(ymax = .data$ymax,
                ymin = .data$ymin, xmax = 4, xmin = 2.6, fill = .data$part)) +
    ggplot2::geom_rect(show.legend = FALSE) +
    ggplot2::coord_polar(theta = "y", start = -pi / 2) +
    ggplot2::xlim(c(1, 4)) + ggplot2::ylim(c(0, 2)) +
    ggplot2::scale_fill_manual(values = c(h2 = "#00AF66", rest = "#ECF0F1")) +
    ggplot2::annotate("text", x = 1, y = 0,
                      label = sprintf("%.0f%%", 100 * h2),
                      size = 6, fontface = "bold", color = "#00875A") +
    ggplot2::labs(title = "Heritability") +
    pb_theme() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank())

  if (requireNamespace("patchwork", quietly = TRUE)) {
    patchwork::wrap_plots(p_means, p_var, p_par, p_h2, ncol = 2) +
      patchwork::plot_annotation(
        title = paste("Genetic analysis dashboard:", trait),
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 16,
                                             color = "#2C3E50")))
  } else {
    list(means = p_means, variance = p_var,
         parameters = p_par, heritability = p_h2)
  }
}
