make_trt <- function() {
  set.seed(3)
  data.frame(
    trt = rep(paste0("T", 1:4), each = 5),
    blk = rep(1:5, times = 4),
    y = c(rnorm(5, 10), rnorm(5, 13), rnorm(5, 16), rnorm(5, 11))
  )
}

test_that("pb_posthoc returns grouped means with letters", {
  df <- make_trt()
  for (m in c("tukey", "duncan", "lsd")) {
    res <- pb_posthoc(df, "y", "trt", "blk", method = m)
    expect_true("groups" %in% names(res$groups))
    expect_equal(res$method, m)
  }
})

test_that("pb_assumptions returns three tests", {
  df <- make_trt()
  a <- pb_assumptions(df, "y", "trt")
  expect_equal(nrow(a), 3)
  expect_true(all(c("Statistic", "p_value") %in% names(a)))
})

test_that("pb_grm returns a symmetric PSD-ish matrix", {
  set.seed(4)
  M <- matrix(sample(0:2, 20 * 50, replace = TRUE), nrow = 20)
  G <- pb_grm(M)
  expect_equal(dim(G), c(20, 20))
  expect_equal(G, t(G), tolerance = 1e-8)
})

test_that("pb_select picks the top fraction", {
  v <- setNames(rnorm(100), paste0("G", 1:100))
  s <- pb_select(v, proportion = 0.1)
  expect_equal(s$n_selected, 10)
  expect_true(all(s$selected >= s$threshold))
  expect_gt(s$selection_differential, 0)
})

test_that("pb_generation_mean solves the additive-dominance model", {
  means <- c(P1 = 20, P2 = 40, F1 = 35, F2 = 32, B1 = 27, B2 = 37)
  vars  <- c(P1 = 1, P2 = 1, F1 = 1, F2 = 2, B1 = 1.5, B2 = 1.5)
  gm <- pb_generation_mean(means, vars)
  expect_named(gm$estimates, c("m", "d", "h"))
  expect_true(all(is.finite(gm$estimates)))
})

test_that("pb_griffing returns GCA effects summing to ~zero", {
  set.seed(5)
  parents <- paste0("P", 1:4)
  crosses <- t(combn(parents, 2))
  df <- data.frame(p1 = crosses[, 1], p2 = crosses[, 2],
                   y = rnorm(nrow(crosses), 50, 5))
  g <- pb_griffing(df, "p1", "p2", "y", method = 2)
  expect_lt(abs(sum(g$gca)), 1e-6)
  expect_equal(g$n_parents, 4)
})
