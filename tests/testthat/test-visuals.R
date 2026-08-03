test_that("pb_palette returns valid hex colours and interpolates", {
  p <- pb_palette("main")
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", p)))
  expect_length(pb_palette("main", 5), 5)
  expect_length(pb_palette("main", 20), 20)   # interpolated beyond base
  expect_length(pb_palette("main", 1), 1)
  expect_equal(pb_palette("main", 3, reverse = TRUE),
               rev(pb_palette("main", 3)))
})

test_that("pb_palette errors on an unknown name", {
  expect_error(pb_palette("nope"), "Unknown palette")
})

test_that("pb_theme and pb_scale return usable ggplot components", {
  expect_s3_class(pb_theme(), "theme")
  expect_s3_class(pb_scale("cool", aesthetics = "fill"), "ScaleDiscrete")
  expect_s3_class(pb_scale("warm", aesthetics = "colour"), "ScaleDiscrete")
})

test_that("upgraded plots still return ggplots with a title", {
  met <- pb_data("met")
  sub <- droplevels(subset(met, env == "E1"))
  res <- pb_analyze(sub, "yield", "gen", "rep", verbose = FALSE)
  for (p in res$plots) {
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(p$labels$title))
  }
})

test_that("pb_gxe_heatmap returns a ggplot for each scaling mode", {
  met <- pb_data("met")
  for (s in c("none", "genotype", "environment")) {
    hm <- pb_gxe_heatmap(met, "gen", "env", "yield", scale = s)
    expect_s3_class(hm, "ggplot")
  }
})

test_that("upgraded heatmaps and GWAS plots render", {
  met <- pb_data("met")
  wide <- stats::reshape(
    stats::aggregate(yield ~ gen + env, met, mean),
    idvar = "gen", timevar = "env", direction = "wide")
  expect_s3_class(pb_heatmap(met, c("yield", "height")), "ggplot")
  expect_s3_class(pb_data_heatmap(met[!duplicated(met$gen), ],
                                  c("yield", "height"), id = "gen"), "ggplot")
})
