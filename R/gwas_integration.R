#' GWAS with rMVP (GLM, MLM, FarmCPU)
#'
#' Runs a genome-wide association scan using the `rMVP` package, which can fit
#' GLM, MLM and FarmCPU models in one call and returns per-marker p-values.
#'
#' @param phe A phenotype data frame; first column = taxa id, second = trait.
#' @param geno Genotype matrix (markers x individuals) or a big.matrix.
#' @param map Marker map data frame with columns SNP, Chr, Pos.
#' @param methods Character vector of models: any of "GLM", "MLM", "FarmCPU".
#' @param nPC Number of principal components to fit as covariates.
#' @param maxLoop FarmCPU iteration cap.
#' @param ncpus Threads.
#' @param file_output Whether rMVP writes plots/files to disk.
#' @return The `rMVP::MVP` result object (contains `$map`, `$glm.results`,
#'   `$mlm.results`, `$farmcpu.results`).
#' @export
pb_gwas_rmvp <- function(phe, geno, map,
                         methods = c("MLM", "FarmCPU"),
                         nPC = 3, maxLoop = 3, ncpus = 1,
                         file_output = FALSE) {
  if (!requireNamespace("rMVP", quietly = TRUE))
    stop("Install the 'rMVP' package to run pb_gwas_rmvp().", call. = FALSE)
  rMVP::MVP(
    phe = phe, geno = geno, map = map,
    nPC.GLM = nPC, nPC.MLM = nPC, nPC.FarmCPU = nPC,
    method = methods, maxLoop = maxLoop, ncpus = ncpus,
    file.output = file_output, verbose = FALSE
  )
}

#' GWAS with GAPIT (GLM, MLM, MLMM, FarmCPU, Blink)
#'
#' Runs a GWAS through the `GAPIT` package. GAPIT is typically installed from
#' GitHub (not CRAN); this wrapper checks for it and forwards the standard
#' hapmap / numeric inputs.
#'
#' @param Y Phenotype data frame: first column taxa, remaining columns traits.
#' @param GD Numeric genotype data frame: first column taxa, then markers.
#' @param GM Marker map data frame: SNP, Chromosome, Position.
#' @param model GWAS model(s): e.g. "MLM", "MLMM", "FarmCPU", "Blink".
#' @param PCA_total Number of principal components to include.
#' @param ... Further arguments passed to [GAPIT::GAPIT()].
#' @return The GAPIT result list.
#' @export
pb_gwas_gapit <- function(Y, GD, GM, model = "MLM", PCA_total = 3, ...) {
  if (!requireNamespace("GAPIT", quietly = TRUE))
    stop("Install 'GAPIT' (github.com/jiabowang/GAPIT) to run pb_gwas_gapit().",
         call. = FALSE)
  GAPIT::GAPIT(Y = Y, GD = GD, GM = GM, model = model,
               PCA.total = PCA_total, ...)
}

#' Build / tidy a marker map
#'
#' Standardises a marker map to the columns most GWAS tools expect
#' (SNP, Chr, Pos), sorts by chromosome then position, and (optionally)
#' computes cumulative genome positions for Manhattan-style plotting.
#'
#' @param data A data frame containing marker, chromosome and position columns.
#' @param snp,chr,pos Column names for marker id, chromosome and position.
#' @param cumulative Add a `cum_pos` column of running genome coordinates.
#' @return A tidied map data frame (SNP, Chr, Pos, and optionally cum_pos).
#' @export
pb_marker_map <- function(data, snp = "SNP", chr = "Chr", pos = "Pos",
                          cumulative = TRUE) {
  .check_columns(data, c(snp, chr, pos))
  m <- data.frame(SNP = data[[snp]], Chr = data[[chr]], Pos = data[[pos]],
                  stringsAsFactors = FALSE)
  m <- m[order(m$Chr, m$Pos), ]
  if (cumulative) {
    offset <- 0
    m$cum_pos <- NA_real_
    for (ch in unique(m$Chr)) {
      idx <- m$Chr == ch
      m$cum_pos[idx] <- m$Pos[idx] + offset
      offset <- max(m$cum_pos[idx], na.rm = TRUE)
    }
  }
  rownames(m) <- NULL
  m
}

#' Manhattan plot from a GWAS results table
#'
#' A lightweight ggplot Manhattan plot given marker positions and p-values,
#' independent of which GWAS engine produced them.
#'
#' @param map A tidied map (from [pb_marker_map()]); needs Chr and cum_pos.
#' @param pvalues Numeric vector of p-values aligned to `map` rows.
#' @param threshold Significance line (raw p, converted to -log10 internally).
#' @return A `ggplot` object.
#' @export
pb_manhattan <- function(map, pvalues, threshold = 5e-8) {
  if (is.null(map$cum_pos))
    map <- pb_marker_map(map, "SNP", "Chr", "Pos", cumulative = TRUE)
  df <- data.frame(cum_pos = map$cum_pos,
                   Chr = as.factor(map$Chr),
                   logp = -log10(pvalues))
  axis_df <- stats::aggregate(cum_pos ~ Chr, df, function(x) mean(range(x)))

  ggplot2::ggplot(df, ggplot2::aes(x = .data$cum_pos, y = .data$logp,
                                   color = .data$Chr)) +
    ggplot2::geom_point(size = 0.9, show.legend = FALSE) +
    ggplot2::geom_hline(yintercept = -log10(threshold),
                        linetype = 2, color = "red") +
    ggplot2::scale_x_continuous(labels = axis_df$Chr, breaks = axis_df$cum_pos) +
    ggplot2::labs(x = "Chromosome", y = expression(-log[10](p)),
                  title = "Manhattan plot") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}

#' QQ plot of GWAS p-values
#'
#' @param pvalues Numeric vector of observed p-values.
#' @return A `ggplot` object comparing observed vs expected -log10(p).
#' @export
pb_qqplot <- function(pvalues) {
  p <- sort(pvalues[!is.na(pvalues)])
  n <- length(p)
  df <- data.frame(
    expected = -log10(stats::ppoints(n)),
    observed = -log10(p)
  )
  ggplot2::ggplot(df, ggplot2::aes(.data$expected, .data$observed)) +
    ggplot2::geom_point(size = 0.9) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linetype = 2) +
    ggplot2::labs(x = expression(Expected~-log[10](p)),
                  y = expression(Observed~-log[10](p)),
                  title = "QQ plot") +
    ggplot2::theme_minimal()
}
