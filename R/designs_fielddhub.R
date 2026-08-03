#' Field layouts with FielDHub (with plot maps)
#'
#' `FielDHub` generates randomised field books *and* the physical plot map
#' (row/column coordinates), which is ideal for teaching how a design is laid
#' out in the field. These wrappers return both the field book and a ggplot
#' field map. They require the `FielDHub` package (a Suggests dependency).
#'
#' Treatment arguments accept either a number (synthetic treatment labels are
#' generated) or a character vector of your own treatment names.
#'
#' @name fielddhub
NULL

#' CRD field layout (FielDHub)
#' @param t Number of treatments, or a character vector of treatment labels.
#' @param reps Number of replications.
#' @param plot_start Starting plot number.
#' @param location Location name.
#' @param seed Optional RNG seed.
#' @return A list with the FielDHub object, field book and a ggplot field map.
#' @rdname fielddhub
#' @export
field_crd <- function(t, reps, plot_start = 101, location = "LOC", seed = NULL) {
  .require_fielddhub("field_crd")
  tt <- if (length(t) == 1 && is.numeric(t)) paste0("T", seq_len(t)) else t
  obj <- FielDHub::CRD(t = tt, reps = reps, plotNumber = plot_start,
                       locationName = location, seed = seed %||% 0)
  .fielddhub_out(obj)
}

#' RCBD field layout (FielDHub)
#' @param t Number of treatments, or a character vector of treatment labels.
#' @param reps Number of blocks/replications.
#' @param plot_start Starting plot number.
#' @param location Location name.
#' @param seed Optional RNG seed.
#' @return A list with the FielDHub object, field book and a ggplot field map.
#' @rdname fielddhub
#' @export
field_rcbd <- function(t, reps, plot_start = 101, location = "LOC", seed = NULL) {
  .require_fielddhub("field_rcbd")
  tt <- if (length(t) == 1 && is.numeric(t)) paste0("T", seq_len(t)) else t
  obj <- FielDHub::RCBD(t = tt, reps = reps, plotNumber = plot_start,
                        locationNames = location, seed = seed %||% 0)
  .fielddhub_out(obj)
}

#' Alpha-lattice field layout (FielDHub)
#' @param t Number of treatments, or a character vector of treatment labels.
#' @param k Size of the incomplete blocks (units per incomplete block).
#' @param r Number of full replicates.
#' @param plot_start Starting plot number.
#' @param location Location name.
#' @param seed Optional RNG seed.
#' @return A list with the FielDHub object, field book and a ggplot field map.
#' @rdname fielddhub
#' @export
field_alpha <- function(t, k, r, plot_start = 101, location = "LOC", seed = NULL) {
  .require_fielddhub("field_alpha")
  tt <- if (length(t) == 1 && is.numeric(t)) paste0("T", seq_len(t)) else t
  obj <- FielDHub::alpha_lattice(t = tt, k = k, r = r, plotNumber = plot_start,
                                 locationNames = location, seed = seed %||% 0)
  .fielddhub_out(obj)
}

#' Augmented RCBD field layout (FielDHub)
#' @param lines Number of unreplicated test lines.
#' @param checks Number of replicated checks.
#' @param b Number of blocks.
#' @param plot_start Starting plot number.
#' @param location Location name.
#' @param seed Optional RNG seed.
#' @return A list with the FielDHub object, field book and a ggplot field map.
#' @rdname fielddhub
#' @export
field_augmented <- function(lines, checks, b, plot_start = 101,
                            location = "LOC", seed = NULL) {
  .require_fielddhub("field_augmented")
  obj <- FielDHub::RCBD_augmented(lines = lines, checks = checks, b = b,
                                  plotNumber = plot_start,
                                  locationNames = location, seed = seed %||% 0)
  .fielddhub_out(obj)
}

#' Draw a field map from any FielDHub field book
#'
#' Renders a ggplot plot-map (ROW x COLUMN) coloured by treatment, with plot
#' numbers as labels — the picture students actually take to the field.
#'
#' @param fieldbook A data frame with ROW, COLUMN, PLOT and TREATMENT columns
#'   (as produced by FielDHub); column matching is case-insensitive.
#' @return A `ggplot` object.
#' @export
field_map <- function(fieldbook) {
  nm <- toupper(names(fieldbook))
  pick <- function(target) {
    hit <- which(nm == target)
    if (length(hit)) fieldbook[[hit[1]]] else NULL
  }
  row <- pick("ROW"); col <- pick("COLUMN")
  trt <- pick("TREATMENT"); if (is.null(trt)) trt <- pick("ENTRY")
  plt <- pick("PLOT")
  if (is.null(row) || is.null(col))
    stop("Field book must contain ROW and COLUMN columns.", call. = FALSE)

  df <- data.frame(ROW = row, COLUMN = col,
                   TRT = as.factor(trt),
                   PLOT = if (is.null(plt)) NA else plt)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$COLUMN, y = .data$ROW,
                                   fill = .data$TRT)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.6) +
    { if (!all(is.na(df$PLOT)))
        ggplot2::geom_text(ggplot2::aes(label = .data$PLOT), size = 3) } +
    ggplot2::scale_y_reverse() +
    ggplot2::labs(x = "Column", y = "Row", fill = "Treatment",
                  title = "Field layout map") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}

# ---- internal helpers ----
.require_fielddhub <- function(fn) {
  if (!requireNamespace("FielDHub", quietly = TRUE))
    stop("`", fn, "()` needs the 'FielDHub' package. ",
         "Install it with install.packages('FielDHub').", call. = FALSE)
}

.fielddhub_out <- function(obj) {
  fb <- obj$fieldBook
  list(design = obj, field_book = fb, field_map = field_map(fb))
}
