#' Principal Component Analysis (FactoMineR)
#'
#' Runs PCA via `FactoMineR::PCA` and returns the model plus ready-to-plot
#' `factoextra` objects (scree, individuals, variables, biplot).
#'
#' @param data A data frame.
#' @param traits Character vector of numeric trait columns.
#' @param groups Optional grouping column name for colouring individuals.
#' @param scale Logical; scale variables to unit variance.
#' @return A list with the `PCA` object, eigenvalues and ggplot objects.
#' @export
pb_pca <- function(data, traits, groups = NULL, scale = TRUE) {
  X <- data[, traits, drop = FALSE]
  res <- FactoMineR::PCA(X, scale.unit = scale, graph = FALSE)
  eig <- factoextra::get_eigenvalue(res)

  p_scree <- factoextra::fviz_eig(res, addlabels = TRUE)
  p_var   <- factoextra::fviz_pca_var(res, col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel = TRUE)
  hab <- if (!is.null(groups)) as.factor(data[[groups]]) else "none"
  p_ind <- factoextra::fviz_pca_ind(res, habillage = hab,
                addEllipses = !is.null(groups), repel = TRUE)
  p_biplot <- factoextra::fviz_pca_biplot(res, repel = TRUE,
                col.var = "#2E9FDF", col.ind = "#696969")

  list(pca = res, eigenvalues = eig,
       plots = list(scree = p_scree, variables = p_var,
                    individuals = p_ind, biplot = p_biplot))
}

#' Hierarchical Clustering on Principal Components (HCPC)
#'
#' `FactoMineR::HCPC` clustering with dendrogram and factor-map plots.
#'
#' @param pca_res A `PCA` object (from [pb_pca()]$pca) or a data frame.
#' @param traits If `pca_res` is a data frame, the trait columns to use.
#' @param clusters Number of clusters (`-1` lets FactoMineR choose).
#' @return A list with the `HCPC` object and factoextra plots.
#' @export
pb_hcpc <- function(pca_res, traits = NULL, clusters = -1) {
  if (is.data.frame(pca_res)) {
    pca_res <- FactoMineR::PCA(pca_res[, traits, drop = FALSE], graph = FALSE)
  }
  hc <- FactoMineR::HCPC(pca_res, nb.clust = clusters, graph = FALSE)
  list(
    hcpc = hc,
    dendrogram = factoextra::fviz_dend(hc, show_labels = TRUE, rect = TRUE),
    cluster_map = factoextra::fviz_cluster(hc, repel = TRUE),
    clusters = hc$data.clust
  )
}

#' Correlation / trait heatmap
#'
#' Produces a clustered correlation heatmap with significance stars using
#' `ggplot2`. Works on a trait matrix or a precomputed correlation matrix.
#'
#' @param data A data frame (if `traits` supplied) or a correlation matrix.
#' @param traits Optional character vector of trait columns.
#' @param method Correlation method.
#' @param lab Logical; print coefficients on tiles.
#' @return A `ggplot` object.
#' @export
pb_heatmap <- function(data, traits = NULL, method = "pearson", lab = TRUE) {
  if (!is.null(traits)) {
    M <- stats::cor(data[, traits, drop = FALSE],
                    use = "pairwise.complete.obs", method = method)
  } else {
    M <- as.matrix(data)
  }
  ord <- stats::hclust(stats::as.dist(1 - M))$order
  M <- M[ord, ord]
  df <- as.data.frame(as.table(M))
  names(df) <- c("Var1", "Var2", "value")

  ggplot2::ggplot(df, ggplot2::aes(.data$Var1, .data$Var2, fill = .data$value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
    { if (lab) ggplot2::geom_text(
        ggplot2::aes(label = sprintf("%.2f", .data$value)), size = 3,
        color = "#2C3E50", fontface = "bold") } +
    ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7",
        high = "#B2182B", midpoint = 0, limits = c(-1, 1), name = "r") +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = "Trait correlation heatmap",
                  subtitle = "Hierarchically ordered; blue = negative, red = positive") +
    pb_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   axis.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank())
}

#' Clustered data heatmap (genotype x trait)
#'
#' Z-score-scaled heatmap of genotypes (rows) by traits (columns) with optional
#' row/column dendrograms, useful for diversity visualisation.
#'
#' @param data A data frame.
#' @param traits Character vector of trait columns.
#' @param id Character genotype id column for row labels.
#' @param scale One of "row", "column", "none".
#' @return A `ggplot` object (long-format tile plot, hierarchically ordered).
#' @export
pb_data_heatmap <- function(data, traits, id = NULL, scale = "column") {
  X <- as.matrix(data[, traits, drop = FALSE])
  if (!is.null(id)) rownames(X) <- data[[id]] else rownames(X) <- seq_len(nrow(X))
  if (scale == "column") X <- scale(X)
  if (scale == "row") X <- t(scale(t(X)))

  ro <- stats::hclust(stats::dist(X))$order
  co <- stats::hclust(stats::dist(t(X)))$order
  X <- X[ro, co, drop = FALSE]

  df <- as.data.frame(as.table(as.matrix(X)))
  names(df) <- c("Genotype", "Trait", "z")
  ggplot2::ggplot(df, ggplot2::aes(.data$Trait, .data$Genotype, fill = .data$z)) +
    ggplot2::geom_tile(color = "grey95", linewidth = 0.3) +
    ggplot2::scale_fill_gradientn(colors = pb_palette("viridis"),
                                  name = "z-score") +
    ggplot2::labs(title = "Genotype \u00d7 trait heatmap",
                  subtitle = "Rows and columns clustered by similarity") +
    pb_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   panel.grid = ggplot2::element_blank())
}

#' Diversity indices for categorical / count data
#'
#' Computes Shannon, Simpson and richness indices per row (e.g. per environment
#' or population) from a matrix of counts or frequencies.
#'
#' @param data A data frame.
#' @param cols Character vector of count/frequency columns.
#' @return A data frame with Richness, Shannon, Simpson and evenness.
#' @export
pb_diversity_indices <- function(data, cols) {
  M <- as.matrix(data[, cols, drop = FALSE])
  P <- M / rowSums(M)
  shannon <- -rowSums(P * log(P), na.rm = TRUE)
  simpson <- 1 - rowSums(P^2, na.rm = TRUE)
  richness <- rowSums(M > 0)
  evenness <- shannon / log(richness)
  data.frame(Richness = richness, Shannon = shannon,
             Simpson = simpson, Evenness = evenness)
}
