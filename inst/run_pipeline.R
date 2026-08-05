#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# PbStatR — full pipeline demonstration
#
# Runs every major function on small simulated datasets so you can confirm the
# whole package executes end-to-end in your environment. Functions that rely on
# optional (Suggests) packages are wrapped in requireNamespace() checks and are
# skipped with a message if the package is not installed.
#
# Usage:  Rscript inst/run_pipeline.R      (from the package root)
# ---------------------------------------------------------------------------

suppressMessages(library(PbStatR))
set.seed(2024)
ok  <- function(x) cat("  [ok]  ", x, "\n")
skip <- function(x) cat("  [skip]", x, "(package not installed)\n")
has <- function(p) requireNamespace(p, quietly = TRUE)

# ---- 0. Bundled example datasets ------------------------------------------
cat("\n== 0. Bundled example datasets ==\n")
for (d in c("met", "geno", "map", "pheno", "augmented"))
  invisible(pb_data(d))
ok("pb_data (met/geno/map/pheno/augmented)")
cat("  causal SNPs in example GWAS data:", paste(pb_data_causal(), collapse = ", "), "\n")

# ---- 0b. Visual theme & palettes ------------------------------------------
cat("\n== 0b. Colourful theme & palettes ==\n")
invisible(pb_palette("main", 8)); invisible(pb_theme())
invisible(pb_scale("cool", aesthetics = "fill"))
ok("pb_palette / pb_theme / pb_scale")

# ---- 1. Single-trait trial data (RCBD) ------------------------------------
cat("\n== 1. Genetic variability & guided analysis ==\n")
rcbd <- data.frame(
  gen = rep(paste0("G", 1:8), each = 3),
  rep = rep(1:3, times = 8),
  yld = rnorm(24, 50, 6)
)
pb_anova(rcbd, "yld", "gen", "rep");                 ok("pb_anova")
pb_genetic_params(rcbd, "yld", "gen", "rep");        ok("pb_genetic_params")
invisible(pb_analyze(rcbd, "yld", "gen", "rep", verbose = FALSE)); ok("pb_analyze")
invisible(pb_explore("heritability")); invisible(pb_workflow("variety_trial"))
ok("pb_explore / pb_workflow (guided helpers)")
pb_genetic_dashboard(rcbd, "yld", "gen", "rep");     ok("pb_genetic_dashboard (one-figure summary)")
pb_summary(rcbd, "yld", "gen");                      ok("pb_summary")
for (m in c("tukey", "duncan", "lsd", "snk", "scheffe"))
  pb_posthoc(rcbd, "yld", "gen", "rep", method = m)
ok("pb_posthoc (5 methods)")
pb_assumptions(rcbd, "yld", "gen");                  ok("pb_assumptions")
# quantitative genetics
pb_varcomp(rcbd, "yld", "gen", "rep");               ok("pb_varcomp")
pb_heritability(rcbd, "yld", "gen", "rep");          ok("pb_heritability")
pb_genetic_advance(rcbd, "yld", "gen", "rep");       ok("pb_genetic_advance")
pb_breeders_eqn(h2 = 0.5, S = 10);                   ok("pb_breeders_eqn")
pb_selection_intensity(0.1);                         ok("pb_selection_intensity")
pb_realized_h2(c(2, 3), c(4, 5));                    ok("pb_realized_h2")
pb_plot_selection(50, 10, 0.1, 0.5);                 ok("pb_plot_selection")
# novel augmented workflow (self-contained, no external package)
aug_chk <- c("C1", "C2", "C3"); aug_new <- paste0("N", 1:9)
aug_dat <- do.call(rbind, lapply(1:3, function(b) {
  data.frame(block = b, gen = c(aug_chk, aug_new[((b-1)*3+1):(b*3)]))
}))
aug_dat$y <- 30 + rep(c(4, 0, -3), length.out = nrow(aug_dat)) + rnorm(nrow(aug_dat), 0, 2)
invisible(pb_aug_analyze(aug_dat, "block", "gen", "y", checks = aug_chk, verbose = FALSE))
ok("pb_aug_analyze (novel augmented workflow + plots)")
# student guide
invisible(pb_guide()); invisible(pb_explain("heritability")); ok("pb_guide / pb_explain")

# ---- 2. Multi-trait data --------------------------------------------------
cat("\n== 2. Associations, diversity, multivariate ==\n")
mt <- data.frame(
  id = paste0("G", 1:30),
  t1 = rnorm(30), t2 = rnorm(30), t3 = rnorm(30), t4 = rnorm(30)
)
mt$yld <- 2 * mt$t1 + mt$t2 + rnorm(30)
traits <- c("t1", "t2", "t3", "t4", "yld")
pb_correlation(mt, traits);                          ok("pb_correlation")
pb_path_analysis(mt, "yld", c("t1", "t2", "t3", "t4")); ok("pb_path_analysis")
pb_diversity(mt, traits, id = "id", clusters = 3);   ok("pb_diversity")
pb_heatmap(mt, traits);                              ok("pb_heatmap (ggplot)")
pb_data_heatmap(mt, traits, id = "id");              ok("pb_data_heatmap (ggplot)")
if (has("FactoMineR") && has("factoextra")) {
  pb_pca(mt, traits); pb_hcpc(mt, traits = traits, clusters = 3); ok("pb_pca / pb_hcpc")
} else skip("pb_pca / pb_hcpc")

# ---- 3. Designs -----------------------------------------------------------
cat("\n== 3. Experimental designs ==\n")
design_crd(paste0("T", 1:5), r = 3);                 ok("design_crd")
design_rcbd(paste0("T", 1:5), r = 3);                ok("design_rcbd")
design_lsd(paste0("T", 1:5));                        ok("design_lsd")
design_alpha_lattice(paste0("T", 1:9), k = 3, r = 2); ok("design_alpha_lattice")
if (has("FielDHub")) {
  fl <- field_rcbd(t = 6, reps = 3); field_map(fl$field_book); ok("field_rcbd + field_map")
} else skip("field_* (FielDHub)")

# publication-ready ANOVA across designs
rcbd_df <- data.frame(gen = rep(paste0("G", 1:6), each = 3),
                      blk = rep(1:3, times = 6), y = rnorm(18, 50, 5))
print(pb_aov_design(rcbd_df, "y", design = "rcbd", treatment = "gen", block = "blk"))
ok("pb_aov_design (rcbd) + print")
fac_df <- expand.grid(A = c("a1", "a2"), B = c("b1", "b2", "b3"), rep = 1:3)
fac_df$y <- rnorm(nrow(fac_df), 20, 3)
pb_aov_design(fac_df, "y", design = "factorial", factors = c("A", "B"), block = "rep")
ok("pb_aov_design (factorial)")
sp_df <- expand.grid(rep = 1:3, N = c("N0", "N1"), V = c("V1", "V2", "V3"))
sp_df$y <- rnorm(nrow(sp_df), 40, 4)
pb_aov_design(sp_df, "y", design = "split", block = "rep", main = "N", sub = "V")
ok("pb_aov_design (split-plot, error strata)")
av <- pb_aov_design(rcbd_df, "y", design = "rcbd", treatment = "gen", block = "blk")
pb_aov_barplot(av); pb_aov_plots(av);                ok("pb_aov_barplot / pb_aov_plots")

# ---- 4. Multi-environment trial data --------------------------------------
cat("\n== 4. MET, stability, and full G x E ==\n")
met <- expand.grid(gen = paste0("G", 1:6), env = paste0("E", 1:5), rep = 1:2)
ge <- setNames(rnorm(6, 0, 3), paste0("G", 1:6))
ee <- setNames(rnorm(5, 0, 5), paste0("E", 1:5))
met$y <- 40 + ge[as.character(met$gen)] + ee[as.character(met$env)] +
  rnorm(nrow(met), 0, 2)

pb_gxe_anova(met, "gen", "env", "rep", "y");         ok("pb_gxe_anova (joint ANOVA)")
pb_gxe_regression(met, "gen", "env", "y");           ok("pb_gxe_regression (Finlay-Wilkinson)")
pb_shukla(met, "gen", "env", "y");                   ok("pb_shukla (stability variance)")
pb_ecovalence(met, "gen", "env", "y");               ok("pb_ecovalence (Wricke)")
pb_stability_ranks(met, "gen", "env", "rep", "y", include_waasb = FALSE); ok("pb_stability_ranks (no WAASB)")
pb_gxe_heatmap(met, "gen", "env", "y", scale = "genotype"); ok("pb_gxe_heatmap (colourful G x E heatmap)")
if (has("metan")) {
  pb_ammi(met, "env", "gen", "rep", "y");            ok("pb_ammi")
  pb_waasb(met, "env", "gen", "rep", "y");           ok("pb_waasb")
  pb_eberhart_russell(met, "env", "gen", "rep", "y"); ok("pb_eberhart_russell")
  pb_gge(met, "gen", "env", "y");                    ok("pb_gge (GGE biplot model)")
  pb_gxe(met, "gen", "env", "rep", "y", methods = "all"); ok("pb_gxe (all methods)")
  pb_stability_ranks(met, "gen", "env", "rep", "y", include_waasb = TRUE); ok("pb_stability_ranks (combined)")
  # multi-trait selection indices & metan plots
  metmt <- expand.grid(GEN = paste0("G", 1:10), ENV = paste0("E", 1:3), REP = 1:3)
  metmt$Y1 <- rnorm(nrow(metmt), 50, 6)
  metmt$Y2 <- rnorm(nrow(metmt), 30, 4)
  metmt$Y3 <- rnorm(nrow(metmt), 100, 10)
  modmt <- metan::gamem_met(metmt, env = ENV, gen = GEN, rep = REP,
                            resp = c(Y1, Y2, Y3), verbose = FALSE)
  pb_mgidi(modmt, SI = 30);                           ok("pb_mgidi (MGIDI)")
  pb_fai_blup(modmt);                                 ok("pb_fai_blup (FAI-BLUP)")
  pb_smith_hazel(modmt);                              ok("pb_smith_hazel (Smith-Hazel)")
  wmt <- metan::waasb(metmt, env = ENV, gen = GEN, rep = REP,
                      resp = c(Y1, Y2, Y3), verbose = FALSE)
  pb_mtsi(wmt);                                       ok("pb_mtsi (MTSI)")
  w1 <- pb_waasb(met, "env", "gen", "rep", "y")
  pb_waasb_xy_plot(w1); pb_waasby_plot(w1); pb_blup_plot(w1)
  ok("pb_waasb_xy_plot / pb_waasby_plot / pb_blup_plot")
} else skip("metan-based stability / GGE / multi-trait")

# ---- 5. Biometrical genetics ----------------------------------------------
cat("\n== 5. Biometrical genetics ==\n")
parents <- paste0("P", 1:5)
crosses <- as.data.frame(t(combn(parents, 2)))
names(crosses) <- c("p1", "p2"); crosses$y <- rnorm(nrow(crosses), 50, 5)
pb_griffing(crosses, "p1", "p2", "y", method = 2);   ok("pb_griffing (diallel)")
pb_generation_mean(
  c(P1 = 20, P2 = 40, F1 = 35, F2 = 32, B1 = 27, B2 = 37),
  c(P1 = 1, P2 = 1, F1 = 1, F2 = 2, B1 = 1.5, B2 = 1.5)); ok("pb_generation_mean")

# ---- 6. Mixed models, genomic selection, ML -------------------------------
cat("\n== 6. Mixed models, genomic selection, machine learning ==\n")
n <- 100; nm <- 400
M <- matrix(sample(0:2, n * nm, replace = TRUE), nrow = n,
            dimnames = list(paste0("G", 1:n), paste0("m", 1:nm)))
eff <- rnorm(nm); g <- scale(M) %*% eff
y <- as.vector(40 + g + rnorm(n, 0, sd(g)))

pb_grm(M);                                           ok("pb_grm (VanRaden GRM)")
blup_df <- data.frame(gen = rownames(M), env = rep(c("A","B"), n/2),
                      y = y)
pb_blup(blup_df, "y", "gen");                        ok("pb_blup (lme4)")
pb_select(setNames(y, rownames(M)), proportion = 0.1); ok("pb_select")
if (has("rrBLUP")) { pb_gblup(y, M, cv_folds = 3); ok("pb_gblup (+CV)") } else skip("pb_gblup")
if (has("randomForest") || has("glmnet")) {
  ms <- intersect(c("gblup", "rf", "glmnet"),
                  c("gblup", if (has("randomForest")) "rf",
                    if (has("glmnet")) "glmnet"))
  pb_ml_compare(y, M, methods = ms, cv_folds = 3);   ok("pb_ml_predict / pb_ml_compare")
} else skip("machine-learning backends")

# ---- 7. Marker maps & GWAS ------------------------------------------------
cat("\n== 7. Marker maps & GWAS ==\n")
map <- pb_marker_map(data.frame(
  SNP = paste0("m", 1:nm), Chr = rep(1:10, each = nm / 10),
  Pos = rep(seq_len(nm / 10), 10) * 1e5))
ok("pb_marker_map")
pv <- runif(nm)
pb_manhattan(map, pv); pb_qqplot(pv);                ok("pb_manhattan / pb_qqplot")
if (has("rMVP")) ok("pb_gwas_rmvp available") else skip("pb_gwas_rmvp (rMVP)")
if (has("GAPIT")) ok("pb_gwas_gapit available") else skip("pb_gwas_gapit (GAPIT)")

# ---- 8. Augmented RCBD ----------------------------------------------------
cat("\n== 8. Augmented RCBD ==\n")
if (has("augmentedRCBD")) {
  aug <- data.frame(
    block = factor(rep(1:3, times = 6)),
    trt = c("C1","C2","C3","N1","N2","N3","C1","C2","C3","N4","N5","N6",
            "C1","C2","C3","N7","N8","N9"),
    y = rnorm(18, 30, 4))
  pb_augmented(aug$block, aug$trt, aug$y, checks = c("C1","C2","C3")); ok("pb_augmented")
} else skip("pb_augmented (augmentedRCBD)")

# ---- 9. Breeding simulation -----------------------------------------------
cat("\n== 9. Breeding pipeline simulation ==\n")
if (has("AlphaSimR")) {
  sim <- pb_sim_founders(n_founders = 60, n_chr = 5, seg_sites = 300,
                         n_qtl = 30, h2 = 0.3)
  run <- pb_sim_pipeline(sim, cycles = 5, n_select = 10, n_cross = 40,
                         method = "phenotypic")
  pb_plot_gain(run);                                 ok("pb_sim_founders / pb_sim_pipeline / pb_plot_gain")
} else skip("breeding simulation (AlphaSimR)")

# ---- 10. Full analysis report ---------------------------------------------
cat("\n== 10. One-click report (pb_report) ==\n")
if (has("rmarkdown") &&
    (Sys.which("pandoc") != "" || rmarkdown::pandoc_available())) {
  met_df <- pb_data("met")
  out <- tempfile(fileext = ".html")
  invisible(pb_report(met_df, "yield", "gen", block = "rep", env = "env",
                      output_file = basename(out), output_dir = dirname(out),
                      format = "html", quiet = TRUE))
  ok("pb_report (multi-environment HTML report rendered)")
} else skip("pb_report (needs rmarkdown + pandoc)")

# ---- 11. Bayesian MET (ProbBreed) -----------------------------------------
cat("\n== 11. Bayesian probability of superiority (ProbBreed) ==\n")
# The custom PbStatR plot works without ProbBreed (synthetic ranking):
rk <- data.frame(Genotype = paste0("G", 1:10),
                 Probability = sort(runif(10), decreasing = TRUE))
pb_bayes_prob_bars(rk, intensity = 0.2, top = 8)
ok("pb_bayes_prob_bars (custom Bayesian plot)")
if (has("ProbBreed")) {
  cat("  ProbBreed available: full bayes_met -> extr_outs -> prob_sup workflow ",
      "can be run (MCMC; slow).\n")
} else skip("pb_bayes_met / extract / prob (ProbBreed + rstan)")

cat("\n== Pipeline demonstration complete ==\n")
