make_rcbd <- function() {
  set.seed(1)
  data.frame(
    gen = rep(paste0("G", 1:5), each = 3),
    rep = rep(1:3, times = 5),
    yld = rnorm(15, 50, 5)
  )
}

test_that("pb_anova returns an ANOVA table and mean squares", {
  df <- make_rcbd()
  res <- pb_anova(df, "yld", "gen", "rep", design = "rcbd")
  expect_true(is.data.frame(res$anova))
  expect_true(is.numeric(res$mean_squares$genotype))
  expect_equal(res$replications, 3)
})

test_that("pb_genetic_params returns expected columns and bounded heritability", {
  df <- make_rcbd()
  gp <- pb_genetic_params(df, "yld", "gen", "rep")
  expect_true(all(c("GCV", "PCV", "ECV", "Heritability", "GAM") %in% names(gp)))
  expect_gte(gp$Heritability, 0)
  expect_lte(gp$Heritability, 1)
  expect_gte(gp$PCV, gp$GCV)  # PCV >= GCV always
})
