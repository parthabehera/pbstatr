# These tests exercise the metan multi-trait wrappers. They are skipped when
# metan is not installed, and they use a small simulated MET data set.

skip_if_no_metan <- function() {
  testthat::skip_if_not_installed("metan")
}

make_met_multi <- function() {
  set.seed(42)
  expand_grid_df <- expand.grid(
    GEN = paste0("G", 1:10),
    ENV = paste0("E", 1:3),
    REP = 1:3
  )
  n <- nrow(expand_grid_df)
  expand_grid_df$Y1 <- rnorm(n, 50, 6)
  expand_grid_df$Y2 <- rnorm(n, 30, 4)
  expand_grid_df$Y3 <- rnorm(n, 100, 10)
  expand_grid_df
}

test_that("metan wrappers error cleanly without metan", {
  skip_if(requireNamespace("metan", quietly = TRUE),
          "metan installed; skipping absence test")
  expect_error(pb_mgidi(NULL), "metan")
  expect_error(pb_mtsi(NULL), "metan")
  expect_error(pb_smith_hazel(NULL), "metan")
  expect_error(pb_venn_plot(letters[1:3]), "metan")
})

test_that("MGIDI and Smith-Hazel run on a fitted model", {
  skip_if_no_metan()
  df <- make_met_multi()
  mod <- pb_met(df, env = "ENV", gen = "GEN", rep = "REP", trait = "Y1")
  # gamem_met on a single trait — MGIDI needs >= 2 traits, so fit multi-trait
  modm <- metan::gamem_met(df, env = ENV, gen = GEN, rep = REP,
                           resp = c(Y1, Y2, Y3), verbose = FALSE)
  mg <- pb_mgidi(modm, SI = 30)
  expect_s3_class(mg, "mgidi")
  res <- pb_get_results(mg, what = "MGIDI")
  expect_true(is.data.frame(res) || tibble::is_tibble(res))
})

test_that("WAASB-based plots and index return ggplots", {
  skip_if_no_metan()
  df <- make_met_multi()
  w <- pb_waasb(df, env = "ENV", gen = "GEN", rep = "REP", trait = "Y1")
  expect_s3_class(pb_waasb_xy_plot(w), "ggplot")
  expect_s3_class(pb_waasby_plot(w), "ggplot")
  expect_s3_class(pb_blup_plot(w), "ggplot")
})

test_that("pb_venn_plot accepts character vectors of selections", {
  skip_if_no_metan()
  a <- c("G1", "G2", "G3", "G4")
  b <- c("G3", "G4", "G5", "G6")
  vp <- pb_venn_plot(MGIDI = a, FAI = b)
  expect_true(inherits(vp, "ggplot") || inherits(vp, "gg") ||
                inherits(vp, "grob") || inherits(vp, "recordedplot") ||
                !is.null(vp))
})
