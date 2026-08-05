test_that("pb_aov_design handles CRD and RCBD", {
  set.seed(1)
  df <- data.frame(
    gen = rep(paste0("G", 1:4), each = 4),
    blk = rep(1:4, times = 4),
    y = rnorm(16, 50, 5))
  crd <- pb_aov_design(df, "y", design = "crd", treatment = "gen")
  expect_s3_class(crd, "pb_aov")
  expect_true("Source" %in% names(crd$anova))
  expect_false(is.na(crd$cv))

  rcbd <- pb_aov_design(df, "y", design = "rcbd", treatment = "gen", block = "blk")
  expect_s3_class(rcbd, "pb_aov")
  expect_true(!is.na(rcbd$r_squared))
  expect_true("sig" %in% names(rcbd$anova))
})

test_that("pb_aov_design handles a Latin square", {
  set.seed(2)
  n <- 4
  df <- expand.grid(row = 1:n, col = 1:n)
  df$trt <- as.vector(sapply(1:n, function(i) ((0:(n-1) + (i-1)) %% n) + 1))
  df$y <- rnorm(n * n, 30, 4)
  lsd <- pb_aov_design(df, "y", design = "lsd",
                       treatment = "trt", row = "row", col = "col")
  expect_s3_class(lsd, "pb_aov")
  expect_true(any(grepl("trt", lsd$anova$Source)))
})

test_that("pb_aov_design handles a factorial", {
  set.seed(3)
  df <- expand.grid(A = c("a1", "a2"), B = c("b1", "b2", "b3"), rep = 1:3)
  df$y <- rnorm(nrow(df), 20, 3)
  fac <- pb_aov_design(df, "y", design = "factorial",
                       factors = c("A", "B"), block = "rep")
  expect_s3_class(fac, "pb_aov")
  # interaction term A:B present
  expect_true(any(grepl("A:B", fac$anova$Source)))
})

test_that("pb_aov_design handles a split-plot with error strata", {
  set.seed(4)
  df <- expand.grid(rep = 1:3, N = c("N0", "N1"), V = c("V1", "V2", "V3"))
  df$y <- rnorm(nrow(df), 40, 4)
  sp <- pb_aov_design(df, "y", design = "split",
                      block = "rep", main = "N", sub = "V")
  expect_s3_class(sp, "pb_aov")
  expect_true("Stratum" %in% names(sp$anova))
  # multiple strata present
  expect_gt(length(unique(sp$anova$Stratum)), 1)
})

test_that("pb_aov_plots and pb_aov_barplot return graphics", {
  set.seed(5)
  df <- data.frame(
    gen = rep(paste0("G", 1:4), each = 4),
    blk = rep(1:4, times = 4),
    y = rnorm(16, 50, 5))
  a <- pb_aov_design(df, "y", design = "rcbd", treatment = "gen", block = "blk")
  bar <- pb_aov_barplot(a)
  expect_s3_class(bar, "ggplot")
  plt <- pb_aov_plots(a)
  # patchwork object or list of ggplots
  expect_true(inherits(plt, "ggplot") || inherits(plt, "patchwork") ||
                is.list(plt))
})

test_that("significance stars are assigned correctly", {
  # internal helper via a known model
  set.seed(6)
  df <- data.frame(
    gen = rep(paste0("G", 1:3), each = 10),
    y = c(rnorm(10, 10), rnorm(10, 30), rnorm(10, 50)))  # strong effect
  a <- pb_aov_design(df, "y", design = "crd", treatment = "gen")
  gen_row <- a$anova[grepl("gen", a$anova$Source), ]
  expect_true(gen_row$sig %in% c("***", "**", "*"))
})
