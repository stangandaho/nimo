#' Permutation-based variable importance
#'
#' Measures how much each predictor contributes to a model: the values of one
#' predictor are randomly shuffled (breaking its relationship with the response),
#' the model is predicted again, and the loss of performance is recorded. The
#' larger the loss, the more the model relies on that predictor. Shuffling is
#' repeated `n_sim` times.
#'
#' Works with every model fitted in nimo: `flexsdm` `fit_*` and `tune_*` models
#' (including [flexsdm::fit_dom()]) and [nm_fit_bioclim()].
#'
#' @param models A fitted model (output of a `fit_*`, `tune_*` or
#' [nm_fit_bioclim()] function) or a list of them. List names are used as model
#' labels; otherwise the algorithm code (e.g. `"glm"`) is used.
#' @param data data.frame with the response and predictors, usually the data used
#' to fit the models.
#' @param response character. Column name with presences (1) and absences or
#' background points (0).
#' @param predictors character. Predictors to evaluate. Default `NULL` uses the
#' predictors of each model.
#' @param n_sim integer. Number of permutations per predictor. Default 10.
#' @param metrics character. Performance metrics used to measure importance; any
#' metric returned by [nm_eval()] (e.g. `"AUC"`, `"TSS"`, `"BOYCE"`, `"IMAE"`,
#' `"JACCARD"`). Default `c("AUC", "TSS")`.
#' @param thr character. Threshold used for threshold-dependent metrics (see
#' [nm_eval()]). Default `"max_sens_spec"`.
#' @param clamp,pred_type Passed to Maxent predictions (see [nm_predict()]).
#'
#' @return A tibble with one row per model, predictor and metric:
#' \itemize{
#' \item model, predictor, metric
#' \item baseline: performance with the original data
#' \item permuted: mean performance after shuffling the predictor
#' \item importance: `baseline - permuted` (mean loss of performance)
#' \item importance_sd: standard deviation of the loss across permutations
#' \item relative: importance in percent of the summed positive importance of the
#' model for that metric
#' }
#'
#' @details Importance is computed on `data`. When it is the training data,
#' importance reflects what the model learnt, not how well it generalises.
#'
#' @seealso [nm_varimp_plot()]
#'
#' @examples
#' \donttest{
#' df <- read.csv(system.file("extdata", "Gyps_africanus_KNP.csv", package = "nimo"))
#' vars <- c("dnw", "dsm", "lst", "ndvi")
#' m_bioc <- nm_fit_bioclim(df, "pr_ab", vars, ".part", thr = "max_sens_spec")
#' m_glm <- flexsdm::fit_glm(df, "pr_ab", vars, partition = ".part", thr = "max_sens_spec")
#' vi <- nm_varimp(list(bioclim = m_bioc, glm = m_glm), data = df, n_sim = 5)
#' vi
#' nm_varimp_plot(vi)
#' }
#'
#' @importFrom dplyr tibble bind_rows
#' @export
nm_varimp <- function(models,
                      data,
                      response = "pr_ab",
                      predictors = NULL,
                      n_sim = 10,
                      metrics = c("AUC", "TSS"),
                      thr = "max_sens_spec",
                      clamp = TRUE,
                      pred_type = "cloglog") {
  if (!is.null(models$model) || !is.null(models$performance)) {
    models <- list(models)
  }
  data <- as.data.frame(data)
  if (!response %in% names(data)) {
    stop("Response column '", response, "' not found in data")
  }
  obs <- data[[response]]
  if (!all(c(0, 1) %in% obs)) {
    stop("data must contain presences (1) and absences or background points (0)")
  }
  if (n_sim < 1) stop("'n_sim' must be at least 1")

  labels <- names(models)
  if (is.null(labels)) labels <- rep("", length(models))
  out <- list()
  for (k in seq_along(models)) {
    m <- models[[k]]$model
    if (is.null(m)) {
      stop("Element ", k, " of 'models' is not a fitted model (no 'model' element)")
    }
    label <- if (nzchar(labels[k])) labels[k] else nm_alg_code(m)
    vars <- predictors
    if (is.null(vars)) {
      vars <- unname(unlist(models[[k]]$predictors))
      vars <- vars[!is.na(vars)]
    }
    missing_vars <- setdiff(vars, names(data))
    if (length(missing_vars) > 0) {
      stop("Predictor(s) not found in data: ", paste(missing_vars, collapse = ", "))
    }

    evaluate <- function(d) {
      p <- nm_predict_df(m, d, clamp = clamp, pred_type = pred_type)
      flexsdm::sdm_eval(p = p[obs == 1], a = p[obs == 0], thr = thr)
    }
    ev <- evaluate(data)
    unknown <- setdiff(metrics, names(ev))
    if (length(unknown) > 0) {
      stop("Unknown metric(s): ", paste(unknown, collapse = ", "))
    }
    perf <- function(d) unlist(evaluate(d)[1, metrics, drop = TRUE])
    baseline <- unlist(ev[1, metrics, drop = TRUE])

    for (v in vars) {
      sims <- vapply(seq_len(n_sim), function(i) {
        d <- data
        d[[v]] <- d[[v]][sample.int(nrow(d))]
        perf(d)
      }, numeric(length(metrics)))
      sims <- matrix(sims, nrow = length(metrics))
      loss <- baseline - sims
      out[[length(out) + 1]] <- tibble(
        model = label,
        predictor = v,
        metric = metrics,
        baseline = unname(baseline),
        permuted = rowMeans(sims),
        importance = rowMeans(loss),
        importance_sd = if (n_sim > 1) apply(loss, 1, stats::sd) else NA_real_
      )
    }
  }
  res <- bind_rows(out)
  key <- paste(res$model, res$metric)
  total <- tapply(pmax(res$importance, 0), key, sum)
  res$relative <- ifelse(total[key] > 0, 100 * pmax(res$importance, 0) / total[key], 0)
  res
}


#' Plot variable importance
#'
#' Bar chart of the output of [nm_varimp()]: one panel per metric, predictors
#' ordered by importance, bars coloured by model, with the permutation standard
#' deviation as error bars.
#'
#' @param varimp tibble returned by [nm_varimp()].
#' @param relative logical. Plot relative importance (percent) instead of the loss
#' of performance. Default `FALSE`.
#'
#' @return A ggplot object.
#' @export
nm_varimp_plot <- function(varimp, relative = FALSE) {
  value <- predictor <- model <- importance_sd <- NULL
  d <- as.data.frame(varimp)
  d$value <- if (relative) d$relative else d$importance
  ord <- names(sort(tapply(d$value, d$predictor, mean)))
  d$predictor <- factor(d$predictor, levels = ord)
  dodge <- ggplot2::position_dodge(width = 0.8)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = value, y = predictor, fill = model)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey60") +
    ggplot2::geom_col(position = dodge, width = 0.75) +
    ggplot2::facet_wrap(~metric, scales = "free_x") +
    ggplot2::labs(x = if (relative) "Relative importance (%)" else "Loss of performance when permuted",
                  y = NULL, fill = "Model") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))
  if (!relative && any(!is.na(d$importance_sd))) {
    p <- p + ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = value - importance_sd, xmax = value + importance_sd),
      position = dodge, height = 0.25, colour = "grey30")
  }
  p
}

#' Short algorithm code of a fitted model (as used by nm_predict)
#' @noRd
nm_alg_code <- function(m) {
  codes <- c(gam = "gam", graf = "gau", glm = "glm", gbm = "gbm", maxnet = "max",
             nnet = "net", randomforest = "raf", ksvm = "svm", domain = "dom",
             nm_bioclim = "bioc")
  cl <- nm_model_class(m)
  if (cl %in% names(codes)) codes[[cl]] else cl
}
