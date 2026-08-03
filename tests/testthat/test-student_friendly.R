make_trial <- function() {
  set.seed(7)
  data.frame(
    variety = rep(paste0("V", 1:6), each = 3),
    block   = rep(1:3, times = 6),
    yield   = c(rnorm(3, 40), rnorm(3, 45), rnorm(3, 50),
                rnorm(3, 42), rnorm(3, 38), rnorm(3, 47))
  )
}

test_that("pb_analyze runs end-to-end and returns all components", {
  df <- make_trial()
  res <- pb_analyze(df, "yield", "variety", "block", verbose = FALSE)
  expect_true(all(c("anova", "assumptions", "genetic_params",
                    "means", "plots") %in% names(res)))
  expect_s3_class(res$plots$boxplot, "ggplot")
  expect_s3_class(res$plots$means_bar, "ggplot")
  expect_s3_class(res$plots$diagnostics, "ggplot")
})

test_that("pb_analyze gives a clear error for a wrong column name", {
  df <- make_trial()
  expect_error(pb_analyze(df, "yld", "variety", "block", verbose = FALSE),
               "not in your data")
})

test_that("pb_summary returns per-genotype descriptive stats", {
  df <- make_trial()
  s <- pb_summary(df, "yield", "variety")
  expect_true(all(c("mean", "sd", "cv", "n") %in% names(s)))
  expect_equal(nrow(s), length(unique(df$variety)))
})

test_that("pb_help prints and returns a menu invisibly", {
  m <- pb_help()
  expect_true(is.data.frame(m))
  expect_true(all(c("Area", "Functions") %in% names(m)))
})

test_that("field_* functions error cleanly when FielDHub is absent", {
  skip_if(requireNamespace("FielDHub", quietly = TRUE),
          "FielDHub installed; skipping absence test")
  expect_error(field_crd(5, 3), "FielDHub")
})
