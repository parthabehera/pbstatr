make_rcbd_qg <- function() {
  set.seed(11)
  data.frame(gen = rep(paste0("G", 1:8), each = 3),
             blk = rep(1:3, times = 8),
             y = rep(rnorm(8, 50, 8), each = 3) + rnorm(24, 0, 2))
}

test_that("pb_varcomp partitions variance with Vp = Vg + Ve", {
  df <- make_rcbd_qg()
  vc <- pb_varcomp(df, "y", "gen", "blk")
  vg <- vc$Variance[1]; ve <- vc$Variance[2]; vp <- vc$Variance[3]
  expect_equal(vp, vg + ve, tolerance = 1e-8)
  expect_true(all(vc$Variance >= 0))
})

test_that("pb_heritability broad-sense is between 0 and 1", {
  df <- make_rcbd_qg()
  h <- pb_heritability(df, "y", "gen", "blk")
  expect_true(all(h$Value >= 0 & h$Value <= 1))
  expect_true(any(grepl("Broad", h$Type)))
})

test_that("pb_heritability narrow-sense from components", {
  h <- pb_heritability(Va = 20, Vd = 5, Ve = 25)
  narrow <- h$Value[h$Type == "Narrow-sense (h2)"]
  expect_equal(narrow, 20 / 50, tolerance = 1e-8)
})

test_that("pb_breeders_eqn: R = h2 * S", {
  r <- pb_breeders_eqn(h2 = 0.5, S = 10)
  expect_equal(r$response, 5)
  r2 <- pb_breeders_eqn(h2 = 0.4, i = 1.755, sigma_p = 10)
  expect_equal(r2$response, 0.4 * 1.755 * 10, tolerance = 1e-6)
})

test_that("pb_selection_intensity matches known values", {
  # for p = 0.1, i is approximately 1.755
  expect_equal(pb_selection_intensity(0.1), 1.755, tolerance = 0.01)
  # for p = 0.05, i is approximately 2.063
  expect_equal(pb_selection_intensity(0.05), 2.063, tolerance = 0.01)
})

test_that("pb_realized_h2 = R / S per cycle", {
  rh <- pb_realized_h2(response = c(2, 3), differential = c(4, 5))
  expect_equal(rh$per_cycle$realized_h2, c(0.5, 0.6), tolerance = 1e-8)
  expect_true(is.numeric(rh$cumulative_h2))
})

test_that("pb_genetic_advance returns GA and GAM", {
  df <- make_rcbd_qg()
  ga <- pb_genetic_advance(df, "y", "gen", "blk")
  expect_true(all(c("GA", "GAM", "H2") %in% names(ga)))
  expect_gte(ga$GAM, 0)
})

test_that("pb_plot_selection returns a ggplot", {
  expect_s3_class(pb_plot_selection(mean = 50, sigma_p = 10, p = 0.1, h2 = 0.5),
                  "ggplot")
})

test_that("pb_aug_analyze recovers block structure and adjusts means", {
  set.seed(1)
  checks <- c("C1", "C2", "C3"); new <- paste0("N", 1:9)
  dat <- do.call(rbind, lapply(1:3, function(b) {
    these <- new[((b - 1) * 3 + 1):(b * 3)]
    data.frame(block = b, gen = c(checks, these))
  }))
  dat$y <- 30 + rep(c(4, 0, -3), length.out = nrow(dat)) +
    rnorm(nrow(dat), 0, 2)
  res <- pb_aug_analyze(dat, "block", "gen", "y", checks = checks,
                        verbose = FALSE)
  expect_s3_class(res, "pb_aug")
  # block effects sum to ~0 (centered on check grand mean)
  expect_lt(abs(sum(res$block_effects)), 1e-6)
  # adjusted means present for all 12 genotypes
  expect_equal(nrow(res$adjusted), 12)
  expect_s3_class(res$plots$adjusted_means, "ggplot")
  expect_s3_class(res$plots$block_effects, "ggplot")
})

test_that("pb_guide and pb_explain return content", {
  g <- pb_guide()
  expect_true(is.data.frame(g))
  expect_true(nrow(g) >= 10)
  ex <- pb_explain("heritability")
  expect_type(ex, "character")
  expect_match(pb_explain("gcv"), "variation", ignore.case = TRUE)
})
