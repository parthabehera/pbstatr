#' Machine-learning genomic prediction
#'
#' Trains a machine-learning model to predict a phenotype from marker data and
#' reports cross-validated predictive ability. Supports random forest
#' (`randomForest`), gradient boosting (`xgboost`), support vector regression
#' (`e1071`), and elastic net (`glmnet`). Each backend is an optional
#' (Suggests) dependency, checked at call time.
#'
#' @param y Numeric phenotype vector.
#' @param M Marker matrix (individuals x markers), rows aligned to `y`.
#' @param method One of "rf", "xgboost", "svm", "glmnet".
#' @param cv_folds Number of cross-validation folds (0 = fit only, no CV).
#' @param seed RNG seed.
#' @param ... Extra arguments passed to the underlying model.
#' @return A list with the fitted model, predictions and (if requested) the
#'   mean cross-validated correlation (predictive ability).
#' @export
pb_ml_predict <- function(y, M, method = c("rf", "xgboost", "svm", "glmnet"),
                          cv_folds = 5, seed = 1, ...) {
  method <- match.arg(method)
  M <- as.matrix(M)
  .require_ml(method)

  fit_fun <- .ml_fitter(method)
  model <- fit_fun(M, y, ...)
  preds <- .ml_predict(method, model, M)

  out <- list(method = method, model = model, fitted = preds)

  if (cv_folds > 1) {
    set.seed(seed)
    obs <- which(!is.na(y))
    folds <- sample(rep(seq_len(cv_folds), length.out = length(obs)))
    acc <- numeric(cv_folds)
    for (k in seq_len(cv_folds)) {
      test <- obs[folds == k]; train <- setdiff(obs, test)
      m <- fit_fun(M[train, , drop = FALSE], y[train], ...)
      p <- .ml_predict(method, m, M[test, , drop = FALSE])
      acc[k] <- stats::cor(p, y[test], use = "complete.obs")
    }
    out$cv_accuracy <- mean(acc)
    out$cv_fold_accuracy <- acc
  }
  out
}

#' Compare several genomic-prediction models
#'
#' Runs GBLUP and any available ML backends on the same data and returns a
#' ranked table of cross-validated predictive ability, so students can see
#' which model wins on their dataset.
#'
#' @param y Numeric phenotype vector.
#' @param M Marker matrix.
#' @param methods Character vector; any of "gblup", "rf", "xgboost", "svm",
#'   "glmnet". Unavailable backends are skipped with a note.
#' @param cv_folds CV folds.
#' @param seed RNG seed.
#' @return A data frame of methods ranked by predictive ability.
#' @export
pb_ml_compare <- function(y, M, methods = c("gblup", "rf", "glmnet"),
                          cv_folds = 5, seed = 1) {
  res <- lapply(methods, function(m) {
    acc <- tryCatch({
      if (m == "gblup") {
        pb_gblup(y, M, cv_folds = cv_folds, seed = seed)$cv_accuracy
      } else {
        pb_ml_predict(y, M, method = m, cv_folds = cv_folds, seed = seed)$cv_accuracy
      }
    }, error = function(e) NA_real_)
    data.frame(Method = m, Predictive_ability = acc)
  })
  out <- do.call(rbind, res)
  out <- out[order(-out$Predictive_ability), ]
  rownames(out) <- NULL
  out
}

# ---- internal ML helpers ----
.require_ml <- function(method) {
  pkg <- switch(method, rf = "randomForest", xgboost = "xgboost",
                svm = "e1071", glmnet = "glmnet")
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("Install the '", pkg, "' package to use method = '", method, "'.",
         call. = FALSE)
}

.ml_fitter <- function(method) {
  switch(method,
    rf = function(X, y, ...) randomForest::randomForest(X, y, ...),
    xgboost = function(X, y, ...)
      xgboost::xgboost(data = X, label = y, nrounds = 50,
                       verbose = 0, ...),
    svm = function(X, y, ...) e1071::svm(X, y, ...),
    glmnet = function(X, y, ...)
      glmnet::cv.glmnet(X, y, alpha = 0.5, ...)
  )
}

.ml_predict <- function(method, model, X) {
  switch(method,
    rf = as.vector(stats::predict(model, X)),
    xgboost = as.vector(stats::predict(model, X)),
    svm = as.vector(stats::predict(model, X)),
    glmnet = as.vector(stats::predict(model, newx = X, s = "lambda.min"))
  )
}
