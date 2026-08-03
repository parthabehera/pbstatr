make_traits <- function() {
  set.seed(2)
  n <- 30
  x1 <- rnorm(n); x2 <- x1 * 0.7 + rnorm(n); y <- x1 * 1.2 + x2 * 0.5 + rnorm(n)
  data.frame(id = paste0("G", 1:n), t1 = x1, t2 = x2, yld = y)
}

test_that("pb_correlation returns a symmetric matrix", {
  df <- make_traits()
  m <- pb_correlation(df, c("t1", "t2", "yld"))
  expect_equal(dim(m), c(3, 3))
  expect_equal(m, t(m))
})

test_that("pb_path_analysis returns direct effects and valid residual", {
  df <- make_traits()
  p <- pb_path_analysis(df, "yld", c("t1", "t2"))
  expect_length(p$direct, 2)
  expect_gte(p$residual, 0)
})

test_that("pb_diversity clusters genotypes", {
  df <- make_traits()
  d <- pb_diversity(df, c("t1", "t2", "yld"), id = "id", clusters = 3)
  expect_length(unique(d$groups), 3)
})
