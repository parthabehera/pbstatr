#' Student & research friendly breeding-analysis guide
#'
#' A learning-oriented menu that maps the whole plant-breeding and genomic
#' analysis journey onto PbStatR functions, grouped by the question a student or
#' researcher is actually asking. Prints a categorised guide and returns it
#' invisibly. Complements [pb_help()] (which lists functions) by organising
#' them into a *workflow* a newcomer can follow end to end.
#'
#' @return Invisibly, a data frame mapping research questions to functions.
#' @export
pb_guide <- function() {
  g <- rbind(
    c("1. Design my experiment",
      "design_* / field_* / pb_aug_analyze",
      "Lay out CRD/RCBD/lattice/augmented trials and draw the field map"),
    c("2. Is my data sound?",
      "pb_assumptions / pb_aov_plots",
      "Check normality, equal variance, and model diagnostics"),
    c("3. Are genotypes different?",
      "pb_aov_design / pb_anova / pb_posthoc",
      "ANOVA for any design + Tukey/Duncan mean grouping"),
    c("4. How heritable is my trait?",
      "pb_varcomp / pb_heritability / pb_genetic_params",
      "Partition variance; broad- & narrow-sense heritability, GCV/PCV"),
    c("5. How much gain can I expect?",
      "pb_genetic_advance / pb_breeders_eqn / pb_plot_selection",
      "Genetic advance (GA, GAM) and the breeder's equation R = h2*S"),
    c("6. How do traits relate?",
      "pb_correlation / pb_path_analysis / pb_heatmap",
      "Correlation, path coefficients, and correlation heatmaps"),
    c("7. Group & explore diversity",
      "pb_diversity / pb_pca / pb_hcpc / pb_data_heatmap",
      "Clustering, PCA, and diversity heatmaps"),
    c("8. Which parents to cross?",
      "pb_line_tester / pb_griffing / pb_generation_mean",
      "Combining ability (GCA/SCA), diallel, gene action"),
    c("9. Test across environments",
      "pb_gxe / pb_ammi / pb_waasb / pb_stability_ranks",
      "G x E, AMMI, WAASB, and a combined stability ranking"),
    c("10. Select on many traits",
      "pb_mgidi / pb_fai_blup / pb_smith_hazel / pb_mtsi",
      "Multi-trait selection indices (MGIDI, FAI-BLUP, Smith-Hazel, MTSI)"),
    c("11. Go genomic",
      "pb_grm / pb_gblup / pb_gwas_rmvp / pb_manhattan / pb_ml_predict",
      "GBLUP, GWAS, Manhattan plots, and ML genomic prediction"),
    c("12. Weigh the risk (Bayesian)",
      "pb_bayes_met / pb_bayes_prob / pb_bayes_prob_bars",
      "Bayesian probability of superior performance & stability"),
    c("13. Simulate a program",
      "pb_sim_founders / pb_sim_pipeline / pb_plot_gain",
      "Simulate selection cycles and track genetic gain"),
    c("14. Report it",
      "pb_analyze / pb_report",
      "One-call guided analysis and a full HTML/Word report")
  )
  df <- as.data.frame(g, stringsAsFactors = FALSE)
  names(df) <- c("Research_question", "Functions", "What_you_learn")
  cat("\n",
      "===============================================================\n",
      "  PbStatR - a research journey from field trial to genome\n",
      "===============================================================\n\n",
      sep = "")
  for (i in seq_len(nrow(df))) {
    cat(sprintf("%s\n    -> %s\n       %s\n\n",
                df$Research_question[i], df$Functions[i], df$What_you_learn[i]))
  }
  cat("New here? Try:  pb_analyze(pb_data(\"met\"), \"yield\", \"gen\", \"rep\")\n")
  cat("Then explore any row above with ?function_name.\n\n")
  invisible(df)
}

#' Explain a genetic parameter in plain language
#'
#' A teaching helper: given the name of a genetic parameter, returns a short,
#' plain-language explanation of what it means, how it is computed, and how to
#' interpret typical values. Handy for students meeting these terms for the
#' first time.
#'
#' @param term One of "heritability", "gcv", "pcv", "gam", "ga", "gca", "sca",
#'   "stability", "blup", "selection". Case-insensitive; partial matches work.
#' @return Invisibly, a character string; also printed.
#' @export
pb_explain <- function(term) {
  defs <- list(
    heritability = paste(
      "Heritability is the share of trait variation that is inherited.",
      "Broad-sense H2 = Vg/Vp; narrow-sense h2 = Va/Vp (only additive).",
      "High (>0.6): selection is effective. Low (<0.3): environment dominates."),
    gcv = paste(
      "Genotypic coefficient of variation = 100*sqrt(Vg)/mean.",
      "Measures heritable variability relative to the mean; higher = more",
      "genetic variation to select from."),
    pcv = paste(
      "Phenotypic coefficient of variation = 100*sqrt(Vp)/mean.",
      "Always >= GCV; a small gap between PCV and GCV means the environment",
      "has little influence, so selection on phenotype works well."),
    gam = paste(
      "Genetic advance as % of mean = 100*GA/mean.",
      "Expected gain from selecting the best fraction, scaled to the mean.",
      "High GAM with high heritability signals additive gene action."),
    ga = paste(
      "Genetic advance GA = k*sqrt(Vp)*H2, the absolute expected improvement",
      "in the next generation when the top fraction (k) is selected."),
    gca = paste(
      "General combining ability: a parent's average performance in crosses.",
      "High GCA = good general parent; reflects additive gene effects."),
    sca = paste(
      "Specific combining ability: how a particular cross deviates from what",
      "its parents' GCAs predict; reflects dominance/epistasis (hybrid value)."),
    stability = paste(
      "Stability = consistent performance across environments. Measured by",
      "regression (bi), Shukla's variance, ecovalence, WAASB. Low variance of",
      "G x E effects = a stable, widely adaptable genotype."),
    blup = paste(
      "Best Linear Unbiased Prediction shrinks genotype estimates toward the",
      "mean by their reliability, giving fairer rankings than raw means,",
      "especially with unbalanced data."),
    selection = paste(
      "Response to selection R = h2*S (breeder's equation). S is the selection",
      "differential (how much better the selected group is). Gain each cycle",
      "grows with heritability and selection intensity."))
  key <- tolower(term)
  hit <- names(defs)[startsWith(names(defs), key)]
  if (!length(hit)) hit <- names(defs)[pmatch(key, names(defs))]
  if (!length(hit) || is.na(hit[1])) {
    msg <- paste0("No explanation for '", term, "'. Try one of: ",
                  paste(names(defs), collapse = ", "), ".")
    cat(msg, "\n"); return(invisible(msg))
  }
  txt <- defs[[hit[1]]]
  cat("\n", toupper(hit[1]), "\n", strrep("-", nchar(hit[1])), "\n", txt, "\n\n",
      sep = "")
  invisible(txt)
}
