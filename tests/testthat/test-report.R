test_that("the report template ships with the package", {
  tmpl <- system.file("rmd", "trial_report.Rmd", package = "PbStatR")
  # During devtools::test() the installed path may be empty; check the source too.
  found <- tmpl != "" || file.exists("../../inst/rmd/trial_report.Rmd")
  expect_true(found)
})

test_that("pb_report validates column names before doing any work", {
  met <- pb_data("met")
  expect_error(
    pb_report(met, "yld", "gen", "rep", format = "html"),
    "not in your data"
  )
})

test_that("pb_report errors cleanly when rmarkdown is unavailable", {
  skip_if(requireNamespace("rmarkdown", quietly = TRUE),
          "rmarkdown installed; skipping absence test")
  met <- pb_data("met")
  expect_error(
    pb_report(met, "yield", "gen", "rep", format = "html"),
    "rmarkdown"
  )
})

test_that("pb_report renders an HTML file end-to-end when possible", {
  skip_if_not_installed("rmarkdown")
  skip_if(Sys.which("pandoc") == "" && !rmarkdown::pandoc_available(),
          "pandoc not available")
  met <- pb_data("met")
  sub <- droplevels(subset(met, env == "E1"))
  out <- tempfile(fileext = ".html")
  res <- pb_report(sub, "yield", "gen", "rep",
                   output_file = basename(out), output_dir = dirname(out),
                   format = "html", quiet = TRUE)
  expect_true(file.exists(res))
  expect_gt(file.size(res), 1000)
})

test_that("pb_report renders a multi-environment report with stability section", {
  skip_if_not_installed("rmarkdown")
  skip_if(Sys.which("pandoc") == "" && !rmarkdown::pandoc_available(),
          "pandoc not available")
  met <- pb_data("met")
  out <- tempfile(fileext = ".html")
  res <- pb_report(met, "yield", "gen", block = "rep", env = "env",
                   output_file = basename(out), output_dir = dirname(out),
                   format = "html", quiet = TRUE)
  expect_true(file.exists(res))
})
