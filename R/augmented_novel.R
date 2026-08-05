#' Augmented design analysis, self-contained (novel workflow)
#'
#' A dependency-free implementation of the augmented block design analysis,
#' woven into PbStatR's genetic-parameter and plotting machinery. Unlike a thin
#' wrapper around the `augmentedRCBD` package, `pb_aug_analyze()` runs the whole
#' story in one call: it recovers the block effects from the replicated checks,
#' adjusts the unreplicated test-genotype means for those block effects,
#' assembles the augmented ANOVA, estimates genetic parameters on the adjusted
#' test entries, and returns ready-to-show plots.
#'
#' The adjustment follows the classical augmented-block logic (Federer): block
#' effects are estimated from checks (which appear in every block), then each
#' test entry's raw value is corrected by its block's deviation.
#'
#' @param data A data frame.
#' @param block Block column name.
#' @param genotype Treatment/genotype column name.
#' @param trait Response trait column name.
#' @param checks Character vector of check names. If NULL, checks are inferred
#'   as genotypes appearing in more than one block.
#' @param verbose Print a short guided summary.
#' @return An object of class `pb_aug` with: `adjusted` (adjusted means for all
#'   entries), `block_effects`, `anova`, `genetic_params` (on the adjusted test
#'   entries), `cv`, and `plots`.
#' @export
#' @examples
#' set.seed(1)
#' checks <- c("C1", "C2", "C3")
#' new <- paste0("N", 1:9)
#' dat <- do.call(rbind, lapply(1:3, function(b) {
#'   these <- new[((b - 1) * 3 + 1):(b * 3)]
#'   data.frame(block = b, gen = c(checks, these))
#' }))
#' dat$y <- 30 + rnorm(nrow(dat), 0, 3) + rep(c(4, 0, -3), length.out = nrow(dat))
#' pb_aug_analyze(dat, "block", "gen", "y", checks = checks)
pb_aug_analyze <- function(data, block, genotype, trait, checks = NULL,
                           verbose = TRUE) {
  d <- data.frame(block = as.factor(data[[block]]),
                  gen = as.factor(data[[genotype]]),
                  y = data[[trait]], stringsAsFactors = FALSE)
  # infer checks = genotypes appearing in > 1 block
  if (is.null(checks)) {
    tab <- table(d$gen, d$block)
    checks <- rownames(tab)[rowSums(tab > 0) > 1]
  }
  d$is_check <- d$gen %in% checks

  # ---- block effects from checks only ----
  chk <- d[d$is_check, ]
  grand_chk <- mean(chk$y, na.rm = TRUE)
  block_eff <- tapply(chk$y, chk$block, mean, na.rm = TRUE) - grand_chk
  block_eff[is.na(block_eff)] <- 0

  # ---- adjust every entry by its block effect ----
  d$adj <- d$y - block_eff[as.character(d$block)]

  # adjusted means: checks average over blocks, test entries are single adj value
  adj_means <- stats::aggregate(adj ~ gen, d, mean)
  adj_means$type <- ifelse(adj_means$gen %in% checks, "check", "test")
  adj_means <- adj_means[order(-adj_means$adj), ]
  names(adj_means) <- c("Genotype", "Adjusted_mean", "Type")
  rownames(adj_means) <- NULL

  # ---- augmented ANOVA (checks + blocks; test entries as residual info) ----
  # Blocks and checks tested against error from check replication.
  aov_df <- chk
  aov_df$block <- droplevels(aov_df$block)
  aov_df$gen <- droplevels(aov_df$gen)
  anova_tab <- tryCatch({
    m <- stats::aov(y ~ block + gen, data = aov_df)
    as.data.frame(stats::anova(m))
  }, error = function(e) NULL)

  mse <- if (!is.null(anova_tab)) anova_tab["Residuals", "Mean Sq"] else NA_real_
  cv <- if (!is.na(mse)) 100 * sqrt(mse) / grand_chk else NA_real_

  # ---- genetic parameters on adjusted TEST entries ----
  test_vals <- adj_means$Adjusted_mean[adj_means$Type == "test"]
  gm <- mean(test_vals, na.rm = TRUE)
  vp <- stats::var(test_vals, na.rm = TRUE)          # among-test-entry variance
  vg <- max(vp - ifelse(is.na(mse), 0, mse), 0)      # subtract error variance
  gp <- data.frame(
    n_test = length(test_vals), Mean = gm,
    Vg = vg, Ve = ifelse(is.na(mse), NA, mse), Vp = vp,
    GCV = 100 * sqrt(vg) / gm, PCV = 100 * sqrt(vp) / gm,
    Heritability = if (vp > 0) vg / vp else NA_real_,
    row.names = NULL)

  plots <- list(
    adjusted_means = .plot_aug_means(adj_means),
    block_effects = .plot_block_eff(block_eff)
  )

  if (verbose) {
    cat("\n=== Augmented design analysis ===\n")
    cat(sprintf("Checks: %s\n", paste(checks, collapse = ", ")))
    cat(sprintf("Blocks: %d  |  Test entries: %d\n",
                nlevels(d$block), gp$n_test))
    if (!is.na(cv)) cat(sprintf("CV (from checks) = %.2f%%\n", cv))
    cat(sprintf("Broad-sense heritability (test entries) = %.2f\n",
                gp$Heritability))
    best <- adj_means$Genotype[adj_means$Type == "test"][1]
    cat(sprintf("Top test entry (adjusted): %s = %.2f\n",
                best, adj_means$Adjusted_mean[adj_means$Genotype == best]))
    cat("Access $adjusted, $anova, $genetic_params, $plots.\n")
    cat("=================================\n\n")
  }

  structure(list(adjusted = adj_means, block_effects = block_eff,
                 anova = anova_tab, genetic_params = gp, cv = cv,
                 checks = checks, plots = plots),
            class = "pb_aug")
}

#' Print method for a pb_aug object
#' @param x A `pb_aug` object.
#' @param ... Ignored.
#' @return The input `x`, invisibly.
#' @export
print.pb_aug <- function(x, ...) {
  cat("<pb_aug> augmented design analysis\n")
  cat(sprintf("  Checks: %s\n", paste(x$checks, collapse = ", ")))
  cat(sprintf("  Test entries: %d | Heritability: %.2f",
              x$genetic_params$n_test, x$genetic_params$Heritability))
  if (!is.na(x$cv)) cat(sprintf(" | CV: %.2f%%", x$cv))
  cat("\n  Access $adjusted, $anova, $genetic_params, $plots.\n")
  invisible(x)
}

# ---- internal plots ----
.plot_aug_means <- function(adj_means) {
  df <- adj_means
  df$Genotype <- factor(df$Genotype, levels = df$Genotype[order(df$Adjusted_mean)])
  ggplot2::ggplot(df, ggplot2::aes(.data$Adjusted_mean, .data$Genotype,
                                   fill = .data$Type)) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::scale_fill_manual(values = c(check = "#E7B800", test = "#2E9FDF"),
                               name = NULL) +
    ggplot2::labs(x = "Adjusted mean", y = "Genotype",
                  title = "Augmented design: adjusted means",
                  subtitle = "Checks (gold) anchor the block adjustment; test entries in blue") +
    pb_theme()
}

.plot_block_eff <- function(block_eff) {
  df <- data.frame(Block = names(block_eff), Effect = as.numeric(block_eff))
  df$Block <- factor(df$Block, levels = df$Block)
  ggplot2::ggplot(df, ggplot2::aes(.data$Block, .data$Effect,
                                   fill = .data$Effect)) +
    ggplot2::geom_col(width = 0.65, show.legend = FALSE) +
    ggplot2::geom_hline(yintercept = 0, color = "#7F8C8D") +
    ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7",
                                  high = "#B2182B", midpoint = 0) +
    ggplot2::labs(x = "Block", y = "Block effect (from checks)",
                  title = "Estimated block effects",
                  subtitle = "Deviations used to adjust the unreplicated test entries") +
    pb_theme()
}
