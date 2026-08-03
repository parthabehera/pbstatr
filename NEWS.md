# PbStatR 0.9.1

* CRAN-readiness pass: added `@return` to all exported functions, switched
  every `ggplot2::aes()` to the `.data$` pronoun (removes global-variable
  NOTEs), qualified `stats::setNames()` calls, removed `LazyData` (no
  `data/` dir), and fixed the DESCRIPTION author/metadata.
* Added CRAN-SUBMISSION.md (submission guide), cran-comments.md, and
  inst/WORDLIST for spell-checking.

# PbStatR 0.9.0

* Polished hex-sticker logo (`man/figures/logo.png`, `logo-hex.png`) with a
  balanced composition: rising sun, GWAS scatter, DNA double helix, and an
  ascending growth-curve bar chart in the package palette.
* All example thumbnails are now PNG for a single consistent format; the
  README gallery and pkgdown homepage reference them directly.
* Reproducible asset scripts: `data-raw/make_figures.R` (plots) and
  `data-raw/make_logo.py` (logo).

# PbStatR 0.8.0

* Added example plot thumbnails in `man/figures/` for the README gallery and
  pkgdown homepage, illustrating the package's plot styles (boxplot, ranked
  means, correlation heatmap, genotype x environment heatmap, mean-vs-stability,
  Manhattan plot, field map, and the palette set).
* New `data-raw/make_figures.R` regenerates these thumbnails from live output.

# PbStatR 0.7.0

* Colourful, consistent visuals across the package: shared `pb_theme()`,
  `pb_palette()` (six colour-blind-friendly palettes) and `pb_scale()`.
* New `pb_gxe_heatmap()` for genotype x environment interaction heatmaps, now
  embedded in multi-environment `pb_report()` output.
* All existing plots restyled with the shared theme and richer colours.

# PbStatR 0.6.0

* `pb_report()` renders a full trial analysis (ANOVA, genetic parameters,
  stability ranking, plots) to a single HTML or Word document.

# PbStatR 0.5.0

* Combined stability ranking (`pb_stability_ranks()`, `pb_plot_stability()`).
* Bundled example datasets via `pb_data()` and a worked GWAS/stability vignette.

# PbStatR 0.4.0

* GWAS engines (rMVP, GAPIT), marker maps, comprehensive G x E, machine-learning
  genomic prediction, and augmentedRCBD integration.
