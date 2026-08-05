test_that("pb_explore returns matching goals and lists all when NULL", {
  all <- pb_explore()
  expect_true(is.data.frame(all))
  expect_gt(nrow(all), 15)
  h <- pb_explore("heritability")
  expect_true(any(grepl("heritability", h$Goal, ignore.case = TRUE)))
  none <- pb_explore("zzz-nonsense")
  expect_equal(nrow(none), 0)
})

test_that("pb_workflow prints a recipe for each study type", {
  for (s in c("variety_trial", "met", "augmented", "gwas",
              "genomic_selection")) {
    r <- pb_workflow(s)
    expect_true(is.character(r))
    expect_gt(length(r), 0)
  }
})

test_that("pb_genetic_dashboard returns a combined plot or list", {
  set.seed(1)
  df <- data.frame(gen = rep(paste0("G", 1:8), each = 3),
                   blk = rep(1:3, times = 8), y = rnorm(24, 50, 6))
  d <- pb_genetic_dashboard(df, "y", "gen", "blk")
  expect_true(inherits(d, "ggplot") || inherits(d, "patchwork") ||
                is.list(d))
  if (is.list(d) && !inherits(d, "ggplot")) {
    expect_true(all(vapply(d, function(p) inherits(p, "ggplot"), logical(1))))
  }
})
