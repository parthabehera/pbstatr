make_met <- function() {
  set.seed(11)
  expand_grid_df <- expand.grid(
    gen = paste0("G", 1:5),
    env = paste0("E", 1:4),
    rep = 1:2
  )
  # genotype effects, environment effects, small interaction, noise
  ge <- setNames(c(2, -1, 0, 3, -2), paste0("G", 1:5))
  ee <- setNames(c(5, -3, 1, -1), paste0("E", 1:4))
  expand_grid_df$y <- 50 +
    ge[as.character(expand_grid_df$gen)] +
    ee[as.character(expand_grid_df$env)] +
    rnorm(nrow(expand_grid_df), 0, 1.5)
  expand_grid_df
}

test_that("pb_gxe_regression: mean bi is ~1 and returns per-genotype rows", {
  df <- make_met()
  reg <- pb_gxe_regression(df, "gen", "env", "y")
  expect_equal(nrow(reg), 5)
  expect_true(all(c("bi", "s2di", "Mean") %in% names(reg)))
  expect_equal(mean(reg$bi), 1, tolerance = 1e-6)
})

test_that("pb_ecovalence percentages sum to 100", {
  df <- make_met()
  eco <- pb_ecovalence(df, "gen", "env", "y")
  expect_equal(sum(eco$Ecovalence_pct), 100, tolerance = 1e-6)
})

test_that("pb_shukla and pb_ecovalence rank genotypes identically", {
  df <- make_met()
  sh <- pb_shukla(df, "gen", "env", "y")
  eco <- pb_ecovalence(df, "gen", "env", "y")
  m <- merge(sh, eco, by = "Genotype")
  expect_equal(order(m$Shukla_sigma2), order(m$Ecovalence))
})

test_that("pb_gxe_anova extracts a GxE p-value", {
  df <- make_met()
  a <- pb_gxe_anova(df, "gen", "env", "rep", "y")
  expect_true(is.numeric(a$gxe_pvalue))
  expect_true("anova" %in% names(a))
})

test_that("pb_marker_map tidies and adds cumulative positions", {
  map <- data.frame(SNP = paste0("m", 1:6),
                    Chr = c(2, 1, 1, 2, 3, 3),
                    Pos = c(50, 10, 30, 20, 5, 40))
  tm <- pb_marker_map(map)
  expect_equal(tm$Chr, sort(tm$Chr))          # sorted by chr
  expect_true("cum_pos" %in% names(tm))
  expect_true(all(diff(tm$cum_pos) >= 0))     # monotonically non-decreasing
})

test_that("pb_manhattan and pb_qqplot return ggplots", {
  map <- pb_marker_map(data.frame(SNP = paste0("m", 1:100),
                    Chr = rep(1:5, each = 20),
                    Pos = rep(seq(1, 20), 5) * 1e5))
  pv <- runif(100)
  expect_s3_class(pb_manhattan(map, pv), "ggplot")
  expect_s3_class(pb_qqplot(pv), "ggplot")
})

test_that("ML and augmented wrappers error cleanly without their packages", {
  skip_if(requireNamespace("randomForest", quietly = TRUE))
  expect_error(pb_ml_predict(rnorm(20), matrix(rnorm(200), 20), method = "rf"),
               "randomForest")
})
