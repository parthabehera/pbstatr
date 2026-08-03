#' Render a full trial-analysis report (HTML or Word)
#'
#' Runs a complete single-trait analysis and renders it into one self-contained
#' report: descriptive summary, ANOVA, assumption checks, genetic variability
#' parameters (GCV, PCV, heritability, GAM), post-hoc mean grouping, and — when
#' the data span multiple environments — a combined G x E stability ranking.
#' All the key figures are embedded.
#'
#' The report is produced from an R Markdown template shipped with the package
#' (`inst/rmd/trial_report.Rmd`) via `rmarkdown::render()`, so it needs the
#' `rmarkdown` package (and, for Word output, a working Pandoc — which RStudio
#' bundles).
#'
#' @param data A data frame.
#' @param trait Response trait column name (e.g. "yield").
#' @param genotype Genotype / treatment column name.
#' @param block Block / replication column name.
#' @param env Optional environment column name. If supplied (and it has more
#'   than one level) the report adds a multi-environment G x E section.
#' @param rep Replication column for the MET section; defaults to `block`.
#' @param format Output format: "html" or "word".
#' @param output_file Path for the rendered file. If NULL, a file named
#'   `PbStatR_report_<trait>.<ext>` is written to `output_dir`.
#' @param output_dir Directory for the output (default: the working directory).
#' @param title,author Report metadata.
#' @param posthoc Post-hoc method for mean grouping (see [pb_posthoc()]).
#' @param quiet Passed to [rmarkdown::render()]; suppresses console chatter.
#' @return The path to the rendered report, invisibly.
#' @export
#' @examples
#' \dontrun{
#' met <- pb_data("met")
#' # single-environment report
#' sub <- subset(met, env == "E1")
#' pb_report(sub, "yield", "gen", "rep", format = "html")
#'
#' # multi-environment report with stability ranking
#' pb_report(met, "yield", "gen", block = "rep", env = "env",
#'           format = "word", title = "Yield trial 2024")
#' }
pb_report <- function(data, trait, genotype, block, env = NULL, rep = block,
                      format = c("html", "word"),
                      output_file = NULL, output_dir = getwd(),
                      title = NULL, author = "PbStatR",
                      posthoc = "tukey", quiet = TRUE) {
  format <- match.arg(format)
  if (!requireNamespace("rmarkdown", quietly = TRUE))
    stop("Install the 'rmarkdown' package to render reports with pb_report().",
         call. = FALSE)

  cols <- c(trait, genotype, block)
  if (!is.null(env)) cols <- c(cols, env, rep)
  .check_columns(data, unique(cols))

  template <- system.file("rmd", "trial_report.Rmd", package = "PbStatR")
  if (template == "")
    stop("Report template not found. Is PbStatR installed correctly?",
         call. = FALSE)

  # Decide whether the MET section is warranted.
  multi_env <- !is.null(env) && length(unique(data[[env]])) > 1L

  out_format <- switch(format,
    html = "html_document",
    word = "word_document"
  )
  ext <- switch(format, html = "html", word = "docx")

  if (is.null(output_file))
    output_file <- sprintf("PbStatR_report_%s.%s",
                           gsub("[^A-Za-z0-9]+", "_", trait), ext)
  if (is.null(title))
    title <- sprintf("Trial analysis: %s", trait)

  # Copy template to a writable temp location (installed dir is read-only).
  tmp_rmd <- tempfile(fileext = ".Rmd")
  file.copy(template, tmp_rmd, overwrite = TRUE)

  params <- list(
    data = data, trait = trait, genotype = genotype, block = block,
    env = env, rep = rep, multi_env = multi_env,
    posthoc = posthoc, title = title, author = author
  )

  rendered <- rmarkdown::render(
    input = tmp_rmd,
    output_format = out_format,
    output_file = output_file,
    output_dir = output_dir,
    params = params,
    envir = new.env(parent = globalenv()),
    quiet = quiet
  )
  message("Report written to: ", rendered)
  invisible(rendered)
}
