#' Quantitative genetics toolkit
#'
#' Core quantitative-genetics estimators that every plant-breeding student
#' needs: narrow- and broad-sense heritability, variance-component
#' partitioning, the breeder's equation (response to selection), genetic gain,
#' realized heritability, and expected genetic advance. These are deliberately
#' transparent, self-contained functions (no heavy dependencies) so learners
#' can see exactly how each quantity is computed.
#'
#' @name quant_genetics
NULL

#' Variance components from a designed experiment
#'
#' Partitions phenotypic variance into genotypic and environmental (error)
#' components from an RCBD, and derives the key ratios. Uses the expected
#' mean squares: Vg = (MSg - MSe) / r, Ve = MSe.
#'
#' @param data A data frame.
#' @param trait Response trait column name.
#' @param genotype Genotype factor column name.
#' @param block Block/replication factor column name.
#' @return A data frame of variance components (Vg, Ve, Vp) and their
#'   proportions, with genotypic and phenotypic coefficients of variation.
#' @rdname quant_genetics
#' @export
#' @examples
#' set.seed(1)
#' df <- data.frame(gen = rep(paste0("G", 1:6), each = 3),
#'                  blk = rep(1:3, times = 6), y = rnorm(18, 50, 6))
#' pb_varcomp(df, "y", "gen", "blk")
pb_varcomp <- function(data, trait, genotype, block) {
  fit <- pb_anova(data, trait, genotype, block, design = "rcbd")
  r <- fit$replications
  ms_g <- fit$mean_squares$genotype
  ms_e <- fit$mean_squares$error
  vg <- max((ms_g - ms_e) / r, 0)
  ve <- ms_e
  vp <- vg + ve
  gm <- mean(data[[trait]], na.rm = TRUE)
  data.frame(
    Component = c("Genotypic (Vg)", "Environmental (Ve)", "Phenotypic (Vp)"),
    Variance = c(vg, ve, vp),
    Proportion = c(vg / vp, ve / vp, 1),
    CV_percent = c(100 * sqrt(vg) / gm, 100 * sqrt(ve) / gm,
                   100 * sqrt(vp) / gm),
    row.names = NULL
  )
}

#' Heritability (broad and narrow sense)
#'
#' Broad-sense heritability H2 = Vg / Vp from an RCBD. If additive and
#' dominance variances are supplied directly (e.g. from a mating design),
#' narrow-sense h2 = Va / Vp is also returned.
#'
#' @param data A data frame (for broad-sense from an RCBD). Optional if `Va`
#'   is supplied.
#' @param trait,genotype,block Column names (for the RCBD route).
#' @param Va,Vd,Ve Optional additive, dominance and environmental variances
#'   (for the component route, e.g. from diallel/NC designs).
#' @return A data frame with the heritability estimate(s) and their basis.
#' @rdname quant_genetics
#' @export
pb_heritability <- function(data = NULL, trait = NULL, genotype = NULL,
                            block = NULL, Va = NULL, Vd = NULL, Ve = NULL) {
  out <- list()
  if (!is.null(data)) {
    vc <- pb_varcomp(data, trait, genotype, block)
    vg <- vc$Variance[vc$Component == "Genotypic (Vg)"]
    vp <- vc$Variance[vc$Component == "Phenotypic (Vp)"]
    out[[length(out) + 1]] <- data.frame(
      Type = "Broad-sense (H2)", Value = vg / vp,
      Basis = "Vg / Vp from RCBD")
  }
  if (!is.null(Va)) {
    vd <- if (is.null(Vd)) 0 else Vd
    ve <- if (is.null(Ve)) 0 else Ve
    vp <- Va + vd + ve
    out[[length(out) + 1]] <- data.frame(
      Type = "Narrow-sense (h2)", Value = Va / vp,
      Basis = "Va / Vp from components")
    out[[length(out) + 1]] <- data.frame(
      Type = "Broad-sense (H2)", Value = (Va + vd) / vp,
      Basis = "(Va + Vd) / Vp from components")
  }
  do.call(rbind, out)
}

#' Breeder's equation: response to selection
#'
#' Predicts the response to selection R = h2 * S (or R = i * h2 * sigma_p),
#' the cornerstone equation of quantitative genetics.
#'
#' @param h2 Narrow-sense heritability (0-1).
#' @param S Selection differential (in trait units). Provide `S`, or `i` and
#'   `sigma_p`.
#' @param i Standardised selection intensity (optional alternative to `S`).
#' @param sigma_p Phenotypic standard deviation (needed with `i`).
#' @return A list with the predicted response, the inputs used, and (if a mean
#'   is derivable) the expected new mean.
#' @rdname quant_genetics
#' @export
pb_breeders_eqn <- function(h2, S = NULL, i = NULL, sigma_p = NULL) {
  if (is.null(S)) {
    if (is.null(i) || is.null(sigma_p))
      stop("Provide either `S`, or both `i` and `sigma_p`.", call. = FALSE)
    S <- i * sigma_p
  }
  R <- h2 * S
  list(response = R, selection_differential = S, heritability = h2,
       selection_intensity = i,
       interpretation = sprintf(
         "Expected gain per cycle = %.3f trait units (h2 = %.2f, S = %.3f).",
         R, h2, S))
}

#' Expected genetic advance and genetic advance as percent of mean
#'
#' GA = k * sqrt(Vp) * H2 and GAM = 100 * GA / mean, the standard selection-
#' gain summary for variability studies.
#'
#' @param data A data frame.
#' @param trait,genotype,block Column names for an RCBD.
#' @param k Selection differential in standard units (default 2.063 = 5%).
#' @return A one-row data frame with Vp, H2, GA and GAM.
#' @rdname quant_genetics
#' @export
pb_genetic_advance <- function(data, trait, genotype, block, k = 2.063) {
  vc <- pb_varcomp(data, trait, genotype, block)
  vg <- vc$Variance[vc$Component == "Genotypic (Vg)"]
  vp <- vc$Variance[vc$Component == "Phenotypic (Vp)"]
  gm <- mean(data[[trait]], na.rm = TRUE)
  h2 <- vg / vp
  ga <- k * sqrt(vp) * h2
  data.frame(Vp = vp, H2 = h2, GA = ga, GAM = 100 * ga / gm, Mean = gm,
             row.names = NULL)
}

#' Realized heritability from a selection experiment
#'
#' h2_realized = R / S, estimated from the observed response `R` and the applied
#' selection differential `S` across one or more cycles.
#'
#' @param response Observed response(s) to selection (numeric vector).
#' @param differential Applied selection differential(s) (same length).
#' @return A data frame of per-cycle realized heritability and the cumulative
#'   (regression-based) estimate.
#' @rdname quant_genetics
#' @export
pb_realized_h2 <- function(response, differential) {
  if (length(response) != length(differential))
    stop("`response` and `differential` must have the same length.",
         call. = FALSE)
  per_cycle <- response / differential
  # cumulative estimate = slope of cumulative R on cumulative S
  cumR <- cumsum(response); cumS <- cumsum(differential)
  slope <- if (length(cumR) > 1) stats::coef(stats::lm(cumR ~ cumS))[2]
           else per_cycle[1]
  list(per_cycle = data.frame(cycle = seq_along(response),
                              response = response, differential = differential,
                              realized_h2 = per_cycle),
       cumulative_h2 = as.numeric(slope))
}

#' Selection intensity from a selected proportion
#'
#' Converts a selected proportion `p` into the standardised selection intensity
#' i (the mean of the truncated standard normal above the truncation point).
#'
#' @param p Selected proportion (0-1).
#' @return The selection intensity i.
#' @rdname quant_genetics
#' @export
pb_selection_intensity <- function(p) {
  if (any(p <= 0 | p >= 1)) stop("`p` must be strictly between 0 and 1.",
                                 call. = FALSE)
  z <- stats::qnorm(1 - p)      # truncation point
  stats::dnorm(z) / p           # i = phi(z) / p
}

#' Plot the breeder's equation and expected gain
#'
#' A simple, student-friendly figure showing the population phenotypic
#' distribution, the selection threshold, the selected group, and the predicted
#' shift of the mean (response to selection) in the next generation.
#'
#' @param mean Population mean.
#' @param sigma_p Phenotypic standard deviation.
#' @param p Selected proportion.
#' @param h2 Narrow-sense heritability.
#' @return A `ggplot` object.
#' @rdname quant_genetics
#' @export
pb_plot_selection <- function(mean = 50, sigma_p = 10, p = 0.1, h2 = 0.5) {
  i <- pb_selection_intensity(p)
  S <- i * sigma_p
  R <- h2 * S
  thr <- mean + stats::qnorm(1 - p) * sigma_p
  x <- seq(mean - 4 * sigma_p, mean + 4 * sigma_p, length.out = 400)
  df <- data.frame(x = x, d = stats::dnorm(x, mean, sigma_p))
  sel <- df[df$x >= thr, ]

  ggplot2::ggplot(df, ggplot2::aes(.data$x, .data$d)) +
    ggplot2::geom_area(fill = "#D6EAF8", color = NA) +
    ggplot2::geom_area(data = sel, fill = "#2E9FDF", alpha = 0.85) +
    ggplot2::geom_vline(xintercept = thr, linetype = 2, color = "#FC4E07",
                        linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = mean, linetype = 3, color = "#7F8C8D") +
    ggplot2::geom_vline(xintercept = mean + R, linetype = 1, color = "#00875A",
                        linewidth = 1) +
    ggplot2::annotate("text", x = mean + R, y = max(df$d) * 0.95,
                      label = sprintf("new mean (+%.1f)", R), hjust = -0.05,
                      size = 3.3, color = "#00875A", fontface = "bold") +
    ggplot2::annotate("text", x = thr, y = max(df$d) * 0.55,
                      label = "selection\nthreshold", hjust = -0.1, size = 3,
                      color = "#FC4E07") +
    ggplot2::labs(x = "Trait value", y = "Density",
                  title = "Response to selection (breeder's equation)",
                  subtitle = sprintf(
                    "Select top %.0f%% -> i = %.2f, S = %.1f, h\u00b2 = %.2f, R = %.1f",
                    100 * p, i, S, h2, R)) +
    pb_theme()
}
