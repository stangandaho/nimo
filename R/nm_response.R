#' Response curves
#'
#' Shows how predicted suitability changes along each predictor.
#'
#' Two types of curves are available:
#' * `"marginal"`: the predictor varies along its range while the other predictors
#' are held at their mean (median level for factors). Same approach as
#' [flexsdm::p_pdp()].
#' * `"partial"`: partial dependence (Friedman 2001). For each value of the
#' predictor, every record of `data` takes that value and predictions are
#' averaged. It accounts for the observed combinations of the other predictors,
#' which matters for models with interactions (e.g. random forest, boosted trees,
#' Bioclim, Domain).
#'
#' Works with every model fitted in nimo: `flexsdm` `fit_*` and `tune_*` models
#' (including [flexsdm::fit_dom()]) and [nm_fit_bioclim()].
#'
#' @param models A fitted model (output of a `fit_*`, `tune_*` or
#' [nm_fit_bioclim()] function) or a list of them. List names are used as model
#' labels; otherwise the algorithm code (e.g. `"glm"`) is used.
#' @param data data.frame with the predictors, usually the data used to fit the
#' models. It sets the range of each predictor and, for `type = "partial"`, the
#' records averaged.
#' @param predictors character. Predictors to plot. Default `NULL` uses the
#' predictors of each model.
#' @param type character. `"marginal"` (default) or `"partial"`.
#' @param resolution integer. Number of values along each quantitative predictor.
#' @param n_max integer. For `type = "partial"`, maximum number of records
#' averaged (a random subset is used above it, to save time). Default 1000.
#' @param clamp,pred_type Passed to Maxent predictions (see [nm_predict()]).
#'
#' @return A tibble with columns model, predictor, value (numeric predictors),
#' level (factor predictors) and suitability.
#'
#' @references Friedman, J. H. (2001). Greedy function approximation: a gradient
#' boosting machine. Annals of Statistics, 29(5), 1189-1232.
#'
#' @seealso [nm_response_plot()], [nm_varimp()]
#'
#' @examples
#' \donttest{
#' df <- read.csv(system.file("extdata", "Gyps_africanus_KNP.csv", package = "nimo"))
#' vars <- c("dnw", "dsm", "lst", "ndvi")
#' m_bioc <- nm_fit_bioclim(df, "pr_ab", vars, ".part")
#' m_glm <- flexsdm::fit_glm(df, "pr_ab", vars, partition = ".part")
#' rc <- nm_response(list(bioclim = m_bioc, glm = m_glm), data = df)
#' nm_response_plot(rc, data = df)
#' }
#'
#' @importFrom dplyr tibble bind_rows
#' @export
nm_response <- function(models,
                        data,
                        predictors = NULL,
                        type = c("marginal", "partial"),
                        resolution = 50,
                        n_max = 1000,
                        clamp = TRUE,
                        pred_type = "cloglog") {
  type <- match.arg(type)
  if (!is.null(models$model) || !is.null(models$performance)) {
    models <- list(models)
  }
  data <- as.data.frame(data)
  if (resolution < 2) stop("'resolution' must be at least 2")

  labels <- names(models)
  if (is.null(labels)) labels <- rep("", length(models))
  out <- list()
  for (k in seq_along(models)) {
    m <- models[[k]]$model
    if (is.null(m)) {
      stop("Element ", k, " of 'models' is not a fitted model (no 'model' element)")
    }
    label <- if (nzchar(labels[k])) labels[k] else nm_alg_code(m)
    model_vars <- unname(unlist(models[[k]]$predictors))
    model_vars <- model_vars[!is.na(model_vars)]
    missing_vars <- setdiff(model_vars, names(data))
    if (length(missing_vars) > 0) {
      stop("Predictor(s) of model '", label, "' not found in data: ",
           paste(missing_vars, collapse = ", "))
    }
    vars <- if (is.null(predictors)) model_vars else intersect(predictors, model_vars)
    x <- stats::na.omit(data[, model_vars, drop = FALSE])

    if (type == "marginal") {
      ref <- x[1, , drop = FALSE]
      for (v in model_vars) {
        if (is.factor(x[[v]]) || is.character(x[[v]])) {
          lv <- sort(unique(x[[v]]))
          ref[[v]] <- lv[max(1, as.integer(length(lv) / 2))]
        } else {
          ref[[v]] <- mean(x[[v]])
        }
      }
    } else {
      ref <- if (nrow(x) > n_max) x[sample.int(nrow(x), n_max), , drop = FALSE] else x
    }

    for (v in vars) {
      categorical <- is.factor(x[[v]]) || is.character(x[[v]])
      grid <- if (categorical) sort(unique(x[[v]])) else
        seq(min(x[[v]]), max(x[[v]]), length.out = resolution)
      newdata <- ref[rep(seq_len(nrow(ref)), times = length(grid)), , drop = FALSE]
      newdata[[v]] <- rep(grid, each = nrow(ref))
      if (is.factor(x[[v]])) newdata[[v]] <- factor(newdata[[v]], levels = levels(x[[v]]))
      pred <- nm_predict_df(m, newdata, clamp = clamp, pred_type = pred_type)
      suit <- colMeans(matrix(pred, nrow = nrow(ref)))
      out[[length(out) + 1]] <- tibble(
        model = label,
        predictor = v,
        value = if (categorical) NA_real_ else grid,
        level = if (categorical) as.character(grid) else NA_character_,
        suitability = suit
      )
    }
  }
  res <- bind_rows(out)
  attr(res, "type") <- type
  res
}


#' Plot response curves
#'
#' Plots the output of [nm_response()]: one panel per predictor, one line per
#' model. Optionally adds the presences (top rug) and absences or background
#' points (bottom rug) of `data`, which shows where the curves are supported by
#' records.
#'
#' @param response_curves tibble returned by [nm_response()].
#' @param data data.frame. Optional records to draw as rugs.
#' @param response character. Column of `data` with presences (1) and absences (0).
#'
#' @return A ggplot object.
#' @export
nm_response_plot <- function(response_curves, data = NULL, response = "pr_ab") {
  value <- suitability <- model <- level <- y <- NULL
  d <- as.data.frame(response_curves)
  num <- d[!is.na(d$value), , drop = FALSE]
  fac <- d[is.na(d$value), , drop = FALSE]
  type <- attr(response_curves, "type")
  if (nrow(num) > 0 && nrow(fac) > 0) {
    message("Categorical predictors are not drawn together with quantitative ones; ",
            "plot them separately, e.g. nm_response_plot(rc[is.na(rc$value), ]).")
    fac <- fac[0, ]
  }

  p <- ggplot2::ggplot() +
    ggplot2::facet_wrap(~predictor, scales = "free_x") +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = NULL, y = "Suitability", colour = "Model",
                  subtitle = if (identical(type, "partial")) "Partial dependence" else
                    "Marginal response (other predictors at their mean)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))

  if (!is.null(data) && response %in% names(data) && nrow(num) > 0) {
    vars <- intersect(unique(num$predictor), names(data))
    rug <- bind_rows(lapply(vars, function(v) {
      if (!is.numeric(data[[v]])) return(NULL)
      tibble(predictor = v, value = data[[v]], y = ifelse(data[[response]] == 1, 1, 0))
    }))
    if (nrow(rug) > 0) {
      p <- p +
        ggplot2::geom_rug(data = rug[rug$y == 1, ], ggplot2::aes(x = value),
                          sides = "t", alpha = 0.3, colour = "#065325") +
        ggplot2::geom_rug(data = rug[rug$y == 0, ], ggplot2::aes(x = value),
                          sides = "b", alpha = 0.3, colour = "grey50") +
        ggplot2::labs(caption = "Rugs: presences (top) and absences / background points (bottom)")
    }
  }
  if (nrow(num) > 0) {
    p <- p + ggplot2::geom_line(data = num, ggplot2::aes(x = value, y = suitability, colour = model),
                                linewidth = 0.9)
  }
  if (nrow(fac) > 0) {
    p <- p + ggplot2::geom_point(data = fac, ggplot2::aes(x = level, y = suitability, colour = model),
                                 size = 2.5, position = ggplot2::position_dodge(width = 0.4))
  }
  p
}
