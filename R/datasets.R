#' Load a bundled example dataset
#'
#' PbStatR ships several ready-to-use example datasets as CSV files so every
#' function in the package can be tried without hunting for data. This helper
#' reads one of them.
#'
#' Available datasets:
#' * `"met"` — balanced multi-environment yield trial: 20 genotypes x 5
#'   environments x 3 reps, columns `gen, env, rep, yield, height`.
#' * `"geno"` — GWAS genotype table: `Taxa` + 500 SNP columns coded 0/1/2
#'   for 200 individuals.
#' * `"map"` — marker map for the SNPs: `SNP, Chr, Pos`.
#' * `"pheno"` — phenotype for the GWAS individuals: `Taxa, trait` (8 SNPs
#'   carry real additive effects, so GWAS recovers signal).
#' * `"augmented"` — augmented RCBD: `block, treatment, yield` with three
#'   replicated checks and twelve unreplicated new lines.
#'
#' @param name One of "met", "geno", "map", "pheno", "augmented".
#' @return A data frame.
#' @export
#' @examples
#' met <- pb_data("met")
#' head(met)
pb_data <- function(name = c("met", "geno", "map", "pheno", "augmented")) {
  name <- match.arg(name)
  file <- system.file("extdata", paste0("pbs_", name, ".csv"),
                      package = "PbStatR")
  if (file == "")
    stop("Example dataset '", name, "' not found. Is PbStatR installed?",
         call. = FALSE)
  df <- utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
  if (name %in% c("met", "augmented")) {
    for (col in intersect(c("gen", "env", "rep", "block", "treatment"),
                          names(df)))
      df[[col]] <- as.factor(df[[col]])
  }
  df
}

#' Names of the causal SNPs in the example GWAS data
#'
#' Returns the marker ids that were given true additive effects when the
#' bundled GWAS phenotype was simulated. Handy for checking whether a GWAS run
#' recovers the known signal.
#'
#' @return A character vector of SNP ids.
#' @export
pb_data_causal <- function() {
  file <- system.file("extdata", "pbs_causal.txt", package = "PbStatR")
  if (file == "") return(character(0))
  readLines(file)
}
