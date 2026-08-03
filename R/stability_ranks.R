#' Combined stability ranking across methods
#'
#' Brings the main stability statistics together into a single table with one
#' row per genotype: mean performance, Finlay-Wilkinson regression coefficient
#' (bi) and deviation (s2di), Shukla's stability variance, Wricke's ecovalence,
#' and (if `metan` is available) the WAASB index. Each statistic is ranked, and
#' a simple average of the stability ranks gives an overall "most stable"
#' ordering — the kind of one-look summary students and breeders want.
#'
#' Ranking convention: rank 1 = most stable. For mean yield, rank 1 = highest
#' yielding. bi is ranked by distance from 1 (ideal adaptability). s2di, Shukla,
#' ecovalence and WAASB are ranked ascending (smaller = more stable).
#'
#' @param data Long-format multi-environment data.
#' @param gen,env,rep Column names for genotype, environment and replication.
#' @param trait Response trait column name.
#' @param include_waasb Add the WAASB index (needs `metan`).
#' @param weight_mean If TRUE, the overall rank averages stability ranks with
#'   the mean-performance rank (a yield-and-stability compromise); if FALSE,
#'   only stability statistics are averaged.
#' @return A data frame ordered by the overall stability ranking, containing the
#'   statistics and their per-method ranks.
#' @export
#' @examples
#' set.seed(1)
#' met <- expand.grid(gen = paste0("G", 1:6),
#'                    env = paste0("E", 1:5), rep = 1:2)
#' met$y <- 40 + rnorm(nrow(met), 0, 3)
#' pb_stability_ranks(met, "gen", "env", "rep", "y", include_waasb = FALSE)
pb_stability_ranks <- function(data, gen, env, rep, trait,
                               include_waasb = TRUE, weight_mean = TRUE) {
  reg <- pb_gxe_regression(data, gen, env, trait)
  sh  <- pb_shukla(data, gen, env, trait)
  eco <- pb_ecovalence(data, gen, env, trait)

  tab <- merge(reg, sh, by = "Genotype")
  tab <- merge(tab, eco[, c("Genotype", "Ecovalence")], by = "Genotype")

  # Optional WAASB from metan
  waasb_ok <- FALSE
  if (include_waasb && requireNamespace("metan", quietly = TRUE)) {
    waasb_ok <- tryCatch({
      w <- pb_waasb(data, env, gen, rep, trait)
      wi <- metan::get_model_data(w, "WAASB")
      names(wi)[names(wi) == gen] <- "Genotype"
      wcol <- setdiff(names(wi), "Genotype")[1]
      tab <- merge(tab, data.frame(Genotype = wi$Genotype,
                                   WAASB = wi[[wcol]]), by = "Genotype")
      TRUE
    }, error = function(e) FALSE)
  }

  # ---- ranks (1 = best/most stable) ----
  tab$rank_mean  <- rank(-tab$Mean, ties.method = "min")
  tab$rank_bi    <- rank(abs(tab$bi - 1), ties.method = "min")
  tab$rank_s2di  <- rank(tab$s2di, ties.method = "min")
  tab$rank_shukla <- rank(tab$Shukla_sigma2, ties.method = "min")
  tab$rank_eco   <- rank(tab$Ecovalence, ties.method = "min")
  stab_cols <- c("rank_bi", "rank_s2di", "rank_shukla", "rank_eco")
  if (waasb_ok) {
    tab$rank_waasb <- rank(tab$WAASB, ties.method = "min")
    stab_cols <- c(stab_cols, "rank_waasb")
  }

  rank_cols <- if (weight_mean) c("rank_mean", stab_cols) else stab_cols
  tab$mean_rank <- rowMeans(tab[, rank_cols, drop = FALSE])
  tab$overall_rank <- rank(tab$mean_rank, ties.method = "min")

  tab <- tab[order(tab$overall_rank), ]
  rownames(tab) <- NULL
  attr(tab, "waasb_included") <- waasb_ok
  attr(tab, "weight_mean") <- weight_mean
  tab
}

#' Plot the combined stability ranking
#'
#' A mean-vs-stability scatter: mean performance on the x-axis against the
#' averaged stability rank on the y-axis (lower = more stable), so the ideal
#' genotypes sit in the lower-right. Points are labelled by genotype.
#'
#' @param ranks Output of [pb_stability_ranks()].
#' @return A `ggplot` object.
#' @export
pb_plot_stability <- function(ranks) {
  ggplot2::ggplot(ranks, ggplot2::aes(x = .data$Mean, y = .data$mean_rank)) +
    ggplot2::geom_point(ggplot2::aes(color = .data$overall_rank,
                                     size = .data$Mean), show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = .data$Genotype),
                       vjust = -0.9, size = 3.3, color = "#2C3E50",
                       fontface = "bold") +
    ggplot2::scale_color_gradientn(colors = pb_palette("main", 8)) +
    ggplot2::scale_size_continuous(range = c(2.5, 6)) +
    ggplot2::scale_y_reverse() +
    ggplot2::labs(x = "Mean performance",
                  y = "Average stability rank (lower = more stable)",
                  title = "Mean vs stability",
                  subtitle = "Ideal genotypes: high mean, low stability rank (lower-right)") +
    pb_theme()
}
