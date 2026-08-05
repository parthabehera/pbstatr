# Tests for the ProbBreed Bayesian wrappers. Full model fitting needs a Stan
# toolchain and is slow, so those paths use skip_if_not_installed and the
# custom-plot path is tested on a synthetic ranking table (no ProbBreed needed).

test_that("Bayesian wrappers error cleanly without ProbBreed", {
  skip_if(requireNamespace("ProbBreed", quietly = TRUE),
          "ProbBreed installed; skipping absence test")
  expect_error(pb_bayes_met(data.frame(), "g", "l", "r", "y"), "ProbBreed")
  expect_error(pb_bayes_extract(NULL), "ProbBreed")
  expect_error(pb_bayes_prob(NULL), "ProbBreed")
})

test_that("pb_bayes_prob_bars builds an attractive ggplot from a ranking", {
  set.seed(1)
  ranking <- data.frame(
    Genotype = paste0("G", 1:10),
    Probability = sort(runif(10), decreasing = TRUE))
  p <- pb_bayes_prob_bars(ranking, intensity = 0.2, top = 8)
  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$labels$title))
  expect_equal(p$labels$x, "Probability of superior performance")
})

test_that("pb_bayes_prob_bars respects the top argument", {
  ranking <- data.frame(
    Genotype = paste0("G", 1:20),
    Probability = sort(runif(20), decreasing = TRUE))
  p_all <- pb_bayes_prob_bars(ranking)
  p_top <- pb_bayes_prob_bars(ranking, top = 5)
  # top plot should have 5 bars in its data
  expect_equal(nrow(p_top$data), 5)
  expect_equal(nrow(p_all$data), 20)
})
