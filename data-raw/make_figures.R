#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Generate example plot thumbnails for the README and pkgdown homepage.
#
# Renders the package's signature plots to man/figures/ as PNGs, overwriting
# the shipped placeholder thumbnails so the repo uses a single, consistent
# PNG format. Run once from the package root (needs the package loaded plus
# ggplot2; a couple of figures also use FielDHub/metan if installed):
#
#   Rscript data-raw/make_figures.R
#
# All eight thumb-*.png files and are referenced by README.md and _pkgdown.yml.
# Re-run whenever the plot styling changes. Uses ragg for crisp text if
# available, otherwise the default PNG device.
# ---------------------------------------------------------------------------

if (requireNamespace("devtools", quietly = TRUE) &&
    file.exists("DESCRIPTION")) {
  devtools::load_all(quiet = TRUE)
} else {
  library(PbStatR)
}
library(ggplot2)

fig_dir <- file.path("man", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

.png_device <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else NULL
save_fig <- function(plot, name, w = 5, h = 3.4, dpi = 140) {
  args <- list(filename = file.path(fig_dir, name), plot = plot,
               width = w, height = h, dpi = dpi, bg = "white")
  if (!is.null(.png_device)) args$device <- .png_device
  do.call(ggsave, args)
  message("wrote ", name)
}

set.seed(2024)
met <- pb_data("met")
sub <- droplevels(subset(met, env == "E1"))

# 1. Boxplot by genotype ----------------------------------------------------
save_fig(PbStatR:::.plot_box(sub, "yield", "gen"), "thumb-boxplot.png")

# 2. Ranked means with grouping letters -------------------------------------
ph <- pb_posthoc(sub, "yield", "gen", "rep", method = "tukey")
save_fig(PbStatR:::.plot_means(ph$groups, "yield"), "thumb-means.png")

# 3. Correlation heatmap ----------------------------------------------------
save_fig(pb_heatmap(met, c("yield", "height")), "thumb-corr-heatmap.png",
         w = 4, h = 3.6)

# 4. Genotype x environment heatmap (centred) -------------------------------
save_fig(pb_gxe_heatmap(met, "gen", "env", "yield", scale = "genotype"),
         "thumb-gxe-heatmap.png", w = 4.6, h = 4.4)

# 5. Mean vs stability ------------------------------------------------------
ranks <- pb_stability_ranks(met, "gen", "env", "rep", "yield",
                            include_waasb = FALSE)
save_fig(pb_plot_stability(ranks), "thumb-stability.png")

# 6. Manhattan plot ---------------------------------------------------------
geno <- pb_data("geno"); map <- pb_data("map"); pheno <- pb_data("pheno")
M <- as.matrix(geno[, -1]); y <- pheno$trait
pv <- apply(M, 2, function(s) {
  if (stats::sd(s) == 0) return(NA_real_)
  summary(stats::lm(y ~ s))$coefficients[2, 4]
})
save_fig(pb_manhattan(pb_marker_map(map), pv, threshold = 0.05 / length(pv)),
         "thumb-manhattan.png", w = 6, h = 3)

# 7. Field layout map (if FielDHub available) -------------------------------
if (requireNamespace("FielDHub", quietly = TRUE)) {
  fl <- field_rcbd(t = 12, reps = 3)
  save_fig(fl$field_map, "thumb-field.png", w = 4.6, h = 4)
}

# 8. Palette swatch ---------------------------------------------------------
pal_df <- do.call(rbind, lapply(
  c("main", "cool", "warm", "field", "viridis", "diverging"),
  function(nm) {
    cols <- pb_palette(nm, 8)
    data.frame(palette = nm, x = seq_along(cols), color = cols)
  }))
pal_df$palette <- factor(pal_df$palette,
  levels = c("main", "cool", "warm", "field", "viridis", "diverging"))
swatch <- ggplot(pal_df, aes(x, palette, fill = color)) +
  geom_tile(color = "white", linewidth = 1) +
  scale_fill_identity() +
  labs(title = "PbStatR palettes") +
  pb_theme() +
  theme(axis.title = element_blank(), axis.text.x = element_blank(),
        panel.grid = element_blank())
save_fig(swatch, "thumb-palettes.png", w = 5, h = 3)

message("\nAll thumbnails written to ", fig_dir)
message("Note: logo.png / logo-hex.png are authored separately in ",
        "data-raw/make_logo.R (SVG-based) \u2014 rerun that if the logo changes.")
