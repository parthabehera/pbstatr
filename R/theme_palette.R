#' PbStatR colour palettes
#'
#' A set of curated, colour-blind-friendly palettes used across the package's
#' plots. Retrieve a palette by name, optionally interpolated to `n` colours.
#'
#' Available palettes:
#' * `"main"` — the default vibrant categorical palette.
#' * `"cool"` — blues/greens/teals.
#' * `"warm"` — reds/oranges/yellows.
#' * `"field"` — earthy greens and golds, for field-layout maps.
#' * `"viridis"` — perceptually uniform, good for continuous fills.
#' * `"diverging"` — blue-white-red, for correlations (centred at zero).
#' * `"manhattan"` — two alternating tones for Manhattan plots.
#' * `"spectral"` — rainbow spectral, ordered continuous emphasis.
#' * `"sunset"` — deep-blue to gold, good for sequential categories.
#'
#' @param name Palette name.
#' @param n Number of colours to return (interpolated if needed). NULL returns
#'   the palette's base colours.
#' @param reverse Reverse the colour order.
#' @return A character vector of hex colours.
#' @export
#' @examples
#' pb_palette("main", 5)
#' pb_palette("diverging", 7)
pb_palette <- function(name = "main", n = NULL, reverse = FALSE) {
  pals <- list(
    main = c("#2E9FDF", "#E7B800", "#FC4E07", "#00AF66", "#8E44AD",
             "#E84393", "#16A085", "#D35400"),
    cool = c("#0B486B", "#3B8686", "#79BD9A", "#A8DBA8", "#CFF09E"),
    warm = c("#7C1D1D", "#B2182B", "#E66519", "#F8C120", "#FDE725"),
    field = c("#1B7837", "#5AAE61", "#A6DBA0", "#D9F0D3", "#E6C200",
              "#C9A227"),
    viridis = c("#440154", "#3B528B", "#21908C", "#5DC863", "#FDE725"),
    diverging = c("#2166AC", "#67A9CF", "#D1E5F0", "#F7F7F7",
                  "#FDDBC7", "#EF8A62", "#B2182B"),
    manhattan = c("#2E5A87", "#5AAE61"),
    spectral = c("#5E4FA2", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598",
                 "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F", "#9E0142"),
    sunset = c("#003F5C", "#58508D", "#BC5090", "#FF6361", "#FFA600")
  )
  cols <- pals[[name]]
  if (is.null(cols)) stop("Unknown palette '", name, "'. Options: ",
                          paste(names(pals), collapse = ", "), call. = FALSE)
  if (reverse) cols <- rev(cols)
  if (!is.null(n)) cols <- grDevices::colorRampPalette(cols)(n)
  cols
}

#' PbStatR ggplot2 theme
#'
#' A clean, modern theme applied to package plots: subtle gridlines, bold
#' coloured title, generous spacing, and readable axis text. Layer it onto any
#' ggplot with `+ pb_theme()`.
#'
#' @param base_size Base font size.
#' @param grid Show light major gridlines.
#' @return A ggplot2 theme object.
#' @export
pb_theme <- function(base_size = 12, grid = TRUE) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 3,
                                         color = "#2C3E50",
                                         margin = ggplot2::margin(b = 6)),
      plot.subtitle = ggplot2::element_text(color = "#5D6D7E",
                                            size = base_size - 1,
                                            margin = ggplot2::margin(b = 8)),
      axis.title = ggplot2::element_text(color = "#34495E", face = "bold"),
      axis.text = ggplot2::element_text(color = "#566573"),
      legend.title = ggplot2::element_text(face = "bold", color = "#34495E"),
      legend.position = "right",
      panel.grid.major = if (grid)
        ggplot2::element_line(color = "#ECF0F1", linewidth = 0.4)
        else ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(12, 12, 12, 12)
    )
}

#' Discrete colour scale using a PbStatR palette
#' @param palette Palette name (see [pb_palette()]).
#' @param reverse Reverse colours.
#' @param ... Passed to [ggplot2::scale_colour_manual()] / fill equivalent.
#' @param aesthetics "colour" or "fill".
#' @return A ggplot2 scale.
#' @export
pb_scale <- function(palette = "main", reverse = FALSE, aesthetics = "fill",
                     ...) {
  cols <- pb_palette(palette, reverse = reverse)
  if (aesthetics == "fill") {
    ggplot2::scale_fill_manual(values = grDevices::colorRampPalette(cols)(256),
                               ...)
  } else {
    ggplot2::scale_colour_manual(values = grDevices::colorRampPalette(cols)(256),
                                 ...)
  }
}
