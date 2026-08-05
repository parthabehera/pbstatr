test_that("pb_data loads all bundled datasets with expected columns", {
  expect_true(all(c("gen", "env", "rep", "yield", "height") %in%
                    names(pb_data("met"))))
  expect_true(all(c("SNP", "Chr", "Pos") %in% names(pb_data("map"))))
  expect_true("Taxa" %in% names(pb_data("geno")))
  expect_true(all(c("Taxa", "trait") %in% names(pb_data("pheno"))))
  expect_true(all(c("block", "treatment", "yield") %in%
                    names(pb_data("augmented"))))
})

test_that("pb_data_causal returns the known causal SNPs", {
  causal <- pb_data_causal()
  expect_type(causal, "character")
  expect_true(length(causal) >= 1)
  expect_true(all(causal %in% pb_data("map")$SNP))
})

test_that("pb_stability_ranks builds a combined, ordered table", {
  met <- pb_data("met")
  r <- pb_stability_ranks(met, "gen", "env", "rep", "yield",
                          include_waasb = FALSE)
  expect_true(all(c("Genotype", "Mean", "bi", "s2di",
                    "Shukla_sigma2", "Ecovalence", "overall_rank") %in% names(r)))
  # ordered by overall_rank ascending
  expect_equal(r$overall_rank, sort(r$overall_rank))
  # mean of Finlay-Wilkinson slopes is ~1
  expect_equal(mean(r$bi), 1, tolerance = 1e-6)
  # every genotype present exactly once
  expect_equal(length(unique(r$Genotype)), nlevels(met$gen))
})

test_that("pb_plot_stability returns a ggplot", {
  met <- pb_data("met")
  r <- pb_stability_ranks(met, "gen", "env", "rep", "yield",
                          include_waasb = FALSE)
  expect_s3_class(pb_plot_stability(r), "ggplot")
})

test_that("GWAS Manhattan recovers structure on bundled data", {
  geno <- pb_data("geno"); map <- pb_data("map"); pheno <- pb_data("pheno")
  M <- as.matrix(geno[, -1]); y <- pheno$trait
  pv <- apply(M, 2, function(s) {
    if (stats::sd(s) == 0) return(NA_real_)
    summary(stats::lm(y ~ s))$coefficients[2, 4]
  })
  tidy <- pb_marker_map(map)
  expect_s3_class(pb_manhattan(tidy, pv), "ggplot")
  # at least one true causal SNP should be among the strongest hits
  top <- names(sort(pv))[1:25]
  expect_true(length(intersect(top, pb_data_causal())) >= 1)
})
