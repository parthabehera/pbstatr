#' Comprehensive genotype x environment (GxE) analysis
#'
#' Estimates GxE effects through every major approach in one call:
#' joint (combined) ANOVA, Finlay-Wilkinson / Eberhart-Russell regression,
#' Shukla's stability variance, Wricke's ecovalence, AMMI, GGE and WAASB.
#' Each component can also be run on its own via the functions it wraps.
#'
#' @param data Long-format multi-environment data.
#' @param gen,env,rep Column names for genotype, environment and replication.
#' @param trait Response trait column name.
#' @param methods Which analyses to run; default "all".
#' @return A named list with one element per requested method.
#' @export
pb_gxe <- function(data, gen, env, rep, trait,
                   methods = c("all", "anova", "regression", "shukla",
                               "ecovalence", "ammi", "gge", "waasb")) {
  methods <- match.arg(methods, several.ok = TRUE)
  if ("all" %in% methods)
    methods <- c("anova", "regression", "shukla", "ecovalence",
                 "ammi", "gge", "waasb")
  out <- list()

  if ("anova" %in% methods)
    out$joint_anova <- pb_gxe_anova(data, gen, env, rep, trait)
  if ("regression" %in% methods)
    out$regression <- pb_gxe_regression(data, gen, env, trait)
  if ("shukla" %in% methods)
    out$shukla <- pb_shukla(data, gen, env, trait)
  if ("ecovalence" %in% methods)
    out$ecovalence <- pb_ecovalence(data, gen, env, trait)
  if ("ammi" %in% methods)
    out$ammi <- pb_ammi(data, env, gen, rep, trait)
  if ("gge" %in% methods)
    out$gge <- pb_gge(data, gen, env, trait)
  if ("waasb" %in% methods)
    out$waasb <- pb_waasb(data, env, gen, rep, trait)

  class(out) <- "pb_gxe"
  out
}

#' Combined (joint) ANOVA across environments
#'
#' Fits trait ~ env + rep(env) + gen + gen:env and partitions the GxE term.
#'
#' @inheritParams pb_gxe
#' @return A list with the ANOVA table and the GxE significance.
#' @export
pb_gxe_anova <- function(data, gen, env, rep, trait) {
  data[[gen]] <- as.factor(data[[gen]])
  data[[env]] <- as.factor(data[[env]])
  data[[rep]] <- as.factor(data[[rep]])
  fml <- stats::as.formula(
    sprintf("%s ~ %s + %s:%s + %s + %s:%s",
            trait, env, env, rep, gen, gen, env))
  model <- stats::aov(fml, data = data)
  tab <- as.data.frame(stats::anova(model))
  gxe_row <- grep(paste0(gen, ":", env), rownames(tab), fixed = TRUE)
  list(anova = tab, model = model,
       gxe_pvalue = if (length(gxe_row)) tab[gxe_row, "Pr(>F)"] else NA_real_)
}

#' Finlay-Wilkinson / Eberhart-Russell joint regression
#'
#' Regresses each genotype's performance on the environmental index (the
#' environment mean), yielding regression coefficient bi (adaptability) and
#' deviation mean square s2di (stability) per genotype.
#'
#' @inheritParams pb_gxe
#' @return A data frame with mean, bi, and s2di per genotype.
#' @export
pb_gxe_regression <- function(data, gen, env, trait) {
  gt <- stats::aggregate(data[[trait]],
          list(gen = data[[gen]], env = data[[env]]), mean, na.rm = TRUE)
  names(gt)[3] <- "y"
  env_index <- stats::aggregate(y ~ env, gt, mean)
  names(env_index)[2] <- "env_mean"
  gt <- merge(gt, env_index, by = "env")

  genos <- unique(gt$gen)
  res <- lapply(genos, function(g) {
    sub <- gt[gt$gen == g, ]
    fit <- stats::lm(y ~ env_mean, data = sub)
    s2di <- sum(stats::residuals(fit)^2) / (nrow(sub) - 2)
    data.frame(Genotype = g, Mean = mean(sub$y),
               bi = stats::coef(fit)[2], s2di = s2di)
  })
  out <- do.call(rbind, res)
  rownames(out) <- NULL
  out
}

#' Shukla's stability variance
#'
#' @inheritParams pb_gxe
#' @return A data frame of Shukla's sigma^2 per genotype (lower = more stable).
#' @export
pb_shukla <- function(data, gen, env, trait) {
  M <- stats::aggregate(data[[trait]],
         list(gen = data[[gen]], env = data[[env]]), mean, na.rm = TRUE)
  W <- stats::reshape(M, idvar = "gen", timevar = "env", direction = "wide")
  rn <- W$gen; W$gen <- NULL; W <- as.matrix(W); rownames(W) <- rn
  g <- nrow(W); e <- ncol(W)
  grand <- mean(W)
  gi <- rowMeans(W) - grand
  ej <- colMeans(W) - grand
  # interaction residuals
  GE <- W - outer(rowMeans(W), colMeans(W), "+") + grand
  ss_ge_i <- rowSums(GE^2)
  sigma2 <- (g * (g - 1) * ss_ge_i - sum(ss_ge_i)) /
            ((g - 1) * (g - 2) * (e - 1))
  data.frame(Genotype = rn, Shukla_sigma2 = sigma2,
             row.names = NULL)
}

#' Wricke's ecovalence
#'
#' @inheritParams pb_gxe
#' @return A data frame of ecovalence (Wi) per genotype and its percentage.
#' @export
pb_ecovalence <- function(data, gen, env, trait) {
  M <- stats::aggregate(data[[trait]],
         list(gen = data[[gen]], env = data[[env]]), mean, na.rm = TRUE)
  W <- stats::reshape(M, idvar = "gen", timevar = "env", direction = "wide")
  rn <- W$gen; W$gen <- NULL; W <- as.matrix(W); rownames(W) <- rn
  grand <- mean(W)
  GE <- W - outer(rowMeans(W), colMeans(W), "+") + grand
  Wi <- rowSums(GE^2)
  data.frame(Genotype = rn, Ecovalence = Wi,
             Ecovalence_pct = 100 * Wi / sum(Wi), row.names = NULL)
}

#' GGE biplot analysis (metan)
#'
#' @inheritParams pb_gxe
#' @return A `metan` gge object (plot with `metan::plot()` / `pb_gge_plot()`).
#' @export
pb_gge <- function(data, gen, env, trait) {
  metan::gge(.data = data,
             env = !!rlang::sym(env),
             gen = !!rlang::sym(gen),
             resp = !!rlang::sym(trait))
}

#' Print method for a pb_gxe result
#' @param x A `pb_gxe` object.
#' @param ... Ignored.
#' @export
print.pb_gxe <- function(x, ...) {
  cat("<pb_gxe> genotype x environment analysis\n")
  cat("Methods computed:", paste(names(x), collapse = ", "), "\n")
  if (!is.null(x$joint_anova))
    cat(sprintf("  GxE interaction p-value: %.4g\n",
                x$joint_anova$gxe_pvalue))
  cat("Access components with $ (e.g. result$regression, result$shukla).\n")
  invisible(x)
}
