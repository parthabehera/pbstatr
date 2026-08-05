# data-raw/make_datasets.R
# Generates the example datasets shipped in data/. Run with:
#   source("data-raw/make_datasets.R")
# then rebuild the package so the .rda files are picked up.

set.seed(2024)

# ---------------------------------------------------------------------------
# 1. pbs_met : a balanced multi-environment yield trial
#    20 genotypes x 5 environments x 3 reps, with real genotype, environment,
#    and genotype-by-environment structure plus noise.
# ---------------------------------------------------------------------------
n_gen <- 20; n_env <- 5; n_rep <- 3
gens <- sprintf("G%02d", seq_len(n_gen))
envs <- paste0("E", seq_len(n_env))

g_eff <- stats::setNames(stats::rnorm(n_gen, 0, 4), gens)     # genotype means
e_eff <- stats::setNames(stats::rnorm(n_env, 0, 6), envs)     # env means
ge_eff <- matrix(stats::rnorm(n_gen * n_env, 0, 2.5),
                 nrow = n_gen, dimnames = list(gens, envs))   # interaction

pbs_met <- expand.grid(gen = gens, env = envs, rep = seq_len(n_rep),
                       stringsAsFactors = FALSE)
pbs_met$yield <- 45 +
  g_eff[pbs_met$gen] +
  e_eff[pbs_met$env] +
  ge_eff[cbind(pbs_met$gen, pbs_met$env)] +
  stats::rnorm(nrow(pbs_met), 0, 2)
# a correlated secondary trait (plant height)
pbs_met$height <- 90 + 0.6 * (pbs_met$yield - 45) +
  g_eff[pbs_met$gen] * 0.5 + stats::rnorm(nrow(pbs_met), 0, 3)
pbs_met$gen <- factor(pbs_met$gen)
pbs_met$env <- factor(pbs_met$env)
pbs_met$rep <- factor(pbs_met$rep)

# ---------------------------------------------------------------------------
# 2. pbs_geno / pbs_map / pbs_pheno : a small GWAS-ready dataset
#    200 individuals, 500 SNPs on 10 chromosomes. A handful of SNPs are given
#    real additive effects on the phenotype so GWAS can recover signal.
# ---------------------------------------------------------------------------
n_ind <- 200; n_snp <- 500; n_chr <- 10
taxa <- sprintf("L%03d", seq_len(n_ind))
snps <- sprintf("snp%03d", seq_len(n_snp))

# genotype matrix coded 0/1/2 with random minor-allele frequencies
maf <- stats::runif(n_snp, 0.05, 0.5)
geno_mat <- sapply(maf, function(p)
  stats::rbinom(n_ind, 2, p))
colnames(geno_mat) <- snps
rownames(geno_mat) <- taxa

# marker map: spread SNPs across chromosomes with increasing positions
chr <- rep(seq_len(n_chr), each = n_snp / n_chr)
pos <- as.vector(sapply(seq_len(n_chr), function(i)
  sort(sample(seq_len(1e7), n_snp / n_chr))))
pbs_map <- data.frame(SNP = snps, Chr = chr, Pos = pos,
                      stringsAsFactors = FALSE)

# assign true effects to 8 causal SNPs
causal <- sample(seq_len(n_snp), 8)
beta <- numeric(n_snp); beta[causal] <- stats::rnorm(8, 0, 2)
g_value <- as.vector(scale(geno_mat) %*% beta)
h2 <- 0.5
ve <- stats::var(g_value) * (1 - h2) / h2
phenotype <- 10 + g_value + stats::rnorm(n_ind, 0, sqrt(ve))

pbs_pheno <- data.frame(Taxa = taxa, trait = phenotype,
                        stringsAsFactors = FALSE)
# genotype in the numeric GD layout (Taxa + one column per SNP), for GAPIT-style
pbs_geno <- data.frame(Taxa = taxa, geno_mat, check.names = FALSE,
                       stringsAsFactors = FALSE)
attr(pbs_geno, "causal_snps") <- snps[causal]

# ---------------------------------------------------------------------------
# 3. pbs_augmented : an augmented RCBD (3 checks replicated, new lines once)
# ---------------------------------------------------------------------------
checks <- c("Check1", "Check2", "Check3")
new_lines <- sprintf("New%02d", 1:12)
blocks <- 3
aug_rows <- do.call(rbind, lapply(seq_len(blocks), function(b) {
  these_new <- new_lines[((b - 1) * 4 + 1):(b * 4)]
  trt <- c(checks, these_new)
  data.frame(block = b, treatment = trt, stringsAsFactors = FALSE)
}))
check_eff <- stats::setNames(c(5, 0, -4), checks)
new_eff <- stats::setNames(stats::rnorm(length(new_lines), 0, 4), new_lines)
block_eff <- stats::setNames(stats::rnorm(blocks, 0, 3), seq_len(blocks))
eff <- ifelse(aug_rows$treatment %in% checks,
              check_eff[aug_rows$treatment],
              new_eff[aug_rows$treatment])
pbs_augmented <- data.frame(
  block = factor(aug_rows$block),
  treatment = factor(aug_rows$treatment),
  yield = 30 + eff + block_eff[as.character(aug_rows$block)] +
    stats::rnorm(nrow(aug_rows), 0, 1.5),
  stringsAsFactors = FALSE
)

# ---- save all ----
usethis::use_data(pbs_met, overwrite = TRUE)
usethis::use_data(pbs_geno, overwrite = TRUE)
usethis::use_data(pbs_map, overwrite = TRUE)
usethis::use_data(pbs_pheno, overwrite = TRUE)
usethis::use_data(pbs_augmented, overwrite = TRUE)
