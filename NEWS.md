# PbStatR 0.9.10

* Set the repository owner to 'parthabehera' across README badges/links,
  DESCRIPTION (URL, BugReports), the pkgdown site URL, and the install command.

# PbStatR 0.9.9

* Redesigned the GitHub landing page (README): hero banner, centred badges,
  a 60-second demo, an icon feature grid, a 6-panel gallery led by the genetic
  dashboard, collapsible feature sections, and a tutorials table.
* Added man/figures/banner.png hero image.

# PbStatR 0.9.8

* New student tutorial vignette ("A complete analysis in five steps") walking
  a real dataset end-to-end: pb_explore() -> pb_analyze() ->
  pb_genetic_dashboard() -> pb_report().

# PbStatR 0.9.7

* Student-research capstone layer that ties the whole package together simply:
  - pb_explore() maps a research goal (heritability, stability, gwas, ...) to
    the right function with a ready example.
  - pb_workflow() prints copy-paste analysis recipes for common study types.
  - pb_genetic_dashboard() renders one attractive multi-panel figure (genotype
    means, variance-partition donut, genetic-parameter bars, heritability gauge).

# PbStatR 0.9.7

* New quantitative-genetics toolkit: pb_varcomp(), pb_heritability() (broad &
  narrow sense), pb_genetic_advance(), pb_breeders_eqn(), pb_realized_h2(),
  pb_selection_intensity(), and the teaching plot pb_plot_selection() that
  visualises the breeder's equation.
* Novel self-contained augmented-design workflow pb_aug_analyze(): recovers
  block effects from checks, adjusts unreplicated test means, and returns
  ANOVA, genetic parameters and plots in one call (no external dependency).
* Student-research-friendly layer: pb_guide() maps the whole field-to-genome
  journey onto functions by research question, and pb_explain() gives
  plain-language definitions of genetic parameters.

# PbStatR 0.9.6

* Integrated the ProbBreed Bayesian multi-environment framework: pb_bayes_met(),
  pb_bayes_extract(), pb_bayes_prob() for the fit -> extract -> probability
  workflow, plus diagnostic/probability plots (pb_bayes_diag_plot(),
  pb_bayes_prob_plot()) and a tidy ranking (pb_bayes_ranking()).
* New attractive, PbStatR-themed pb_bayes_prob_bars() plot of the probability of
  superior performance with a selection-intensity reference line.

# PbStatR 0.9.5

* Package-wide figure polish for more attractive, publication-ready plots:
  - Manhattan plot now uses clean alternating two-tone chromosomes,
    highlights genome-wide-significant hits in red, and adds a suggestive line.
  - QQ plot gains a 95%% confidence band and the genomic inflation factor
    (lambda).
  - Mean-vs-stability plot adds a shaded "ideal" quadrant and mean guide lines.
  - Correlation heatmap uses adaptive (white/dark) label contrast.
  - Explicit x/y axis titles added to all heatmaps.
* New palettes: `manhattan`, `spectral`, and `sunset` (see `pb_palette()`).

# PbStatR 0.9.4

* New `pb_aov_design()`: publication-ready ANOVA for all standard designs
  (CRD, RCBD, Latin square, factorial, split-plot, split-split-plot,
  strip-plot) with correct error strata, a formatted table (significance
  stars, CV, R-squared, grand mean), and a print method.
* New diagnostic and summary plots: `pb_aov_plots()` (residuals-vs-fitted,
  Q-Q, scale-location, residual histogram) and `pb_aov_barplot()` (variance
  partition). patchwork added to Suggests for the combined panel.

# PbStatR 0.9.3

* Integrated the full metan multi-trait toolkit: `pb_mgidi()` (MGIDI),
  `pb_mtsi()` (MTSI), `pb_fai_blup()` (FAI-BLUP) and `pb_smith_hazel()`
  (Smith-Hazel), plus `pb_get_results()` to extract tidy result tables.
* Added metan visualisations: `pb_radar_plot()`, `pb_venn_plot()`,
  `pb_waasb_xy_plot()`, `pb_waasby_plot()` and `pb_blup_plot()`.

# PbStatR 0.9.2

* Continuous integration via GitHub Actions: `R-CMD-check.yaml` runs on
  Windows, macOS and Linux (R release, devel, oldrel-1); plus `test-coverage`
  (Codecov), `lint` (lintr), and `pkgdown` site deployment. Configured so the
  GitHub-only GAPIT suggest does not break CI.
* Added CI status badges and a `.lintr` config.

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
