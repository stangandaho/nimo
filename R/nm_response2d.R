#' Two-predictor response surface
#'
#' Shows how predicted suitability changes over the combinations of two
#' quantitative predictors, which reveals interactions between them. Like
#' [nm_response()], the surface is either `"marginal"` (other predictors held at
#' their mean, as in [flexsdm::p_bpdp()]) or `"partial"` (predictions averaged
#' over the records of `data`).
#'
#' Works with every model fitted in nimo: `flexsdm` `fit_*` and `tune_*` models
#' (including [flexsdm::fit_dom()]) and [nm_fit_bioclim()].
#'
#' @param models A fitted model or a list of them (see [nm_response()]).
#' @param data data.frame with the predictors, usually the data used to fit the
#' models. It sets the range of each predictor and, for `type = "partial"`, the
#' records averaged.
#' @param predictors character. The two quantitative predictors to cross.
#' @param type character. `"marginal"` (default) or `"partial"`.
#' @param resolution integer. Number of values along each predictor (the surface
#' has `resolution^2` cells).
#' @param n_max integer. For `type = "partial"`, maximum number of records
#' averaged. Default 200 (partial surfaces predict `resolution^2 * n_max` rows).
#' @param clamp,pred_type Passed to Maxent predictions (see [nm_predict()]).
#'
#' @return A tibble with columns model, the two predictors and suitability. The
#' attribute `"boundary"` holds the convex hull of the records in the two
#' predictors (the region supported by data).
#'
#' @seealso [nm_response2d_plot()], [nm_response()]
#'
#' @examples
#' \donttest{
#' df <- read.csv(system.file("extdata", "Gyps_africanus_KNP.csv", package = "nimo"))
#' m_raf <- flexsdm::fit_raf(df, "pr_ab", c("dnw", "dsm", "lst", "ndvi"), partition = ".part")
#' rs <- nm_response2d(m_raf, df, predictors = c("lst", "ndvi"))
#' nm_response2d_plot(rs)
#' }
#'
#' @importFrom dplyr tibble bind_rows
#' @export
nm_response2d <- function(models,
                          data,
                          predictors,
                          type = c("marginal", "partial"),
                          resolution = 50,
                          n_max = 200,
                          clamp = TRUE,
                          pred_type = "cloglog") {
  type <- match.arg(type)
  if (!is.null(models$model) || !is.null(models$performance)) {
    models <- list(models)
  }
  if (length(predictors) != 2) stop("'predictors' must be two predictor names")
  if (resolution < 2) stop("'resolution' must be at least 2")
  data <- as.data.frame(data)

  labels <- names(models)
  if (is.null(labels)) labels <- rep("", length(models))
  out <- list()
  boundary <- NULL
  for (k in seq_along(models)) {
    m <- models[[k]]$model
    if (is.null(m)) {
      stop("Element ", k, " of 'models' is not a fitted model (no 'model' element)")
    }
    label <- if (nzchar(labels[k])) labels[k] else nm_alg_code(m)
    model_vars <- unname(unlist(models[[k]]$predictors))
    model_vars <- model_vars[!is.na(model_vars)]
    if (!all(predictors %in% model_vars)) {
      stop("Model '", label, "' does not use predictor(s): ",
           paste(setdiff(predictors, model_vars), collapse = ", "))
    }
    missing_vars <- setdiff(model_vars, names(data))
    if (length(missing_vars) > 0) {
      stop("Predictor(s) of model '", label, "' not found in data: ",
           paste(missing_vars, collapse = ", "))
    }
    x <- stats::na.omit(data[, model_vars, drop = FALSE])
    if (!all(vapply(x[predictors], is.numeric, logical(1)))) {
      stop("Both predictors must be quantitative")
    }

    if (type == "marginal") {
      ref <- x[1, , drop = FALSE]
      for (v in model_vars) {
        if (is.numeric(x[[v]])) {
          ref[[v]] <- mean(x[[v]])
        } else {
          lv <- sort(unique(x[[v]]))
          ref[[v]] <- lv[max(1, as.integer(length(lv) / 2))]
        }
      }
    } else {
      ref <- if (nrow(x) > n_max) x[sample.int(nrow(x), n_max), , drop = FALSE] else x
    }

    grid <- expand.grid(
      seq(min(x[[predictors[1]]]), max(x[[predictors[1]]]), length.out = resolution),
      seq(min(x[[predictors[2]]]), max(x[[predictors[2]]]), length.out = resolution)
    )
    names(grid) <- predictors
    newdata <- ref[rep(seq_len(nrow(ref)), times = nrow(grid)), , drop = FALSE]
    newdata[[predictors[1]]] <- rep(grid[[1]], each = nrow(ref))
    newdata[[predictors[2]]] <- rep(grid[[2]], each = nrow(ref))
    pred <- nm_predict_df(m, newdata, clamp = clamp, pred_type = pred_type)
    grid$suitability <- colMeans(matrix(pred, nrow = nrow(ref)))
    out[[k]] <- tibble(model = label, grid)

    if (is.null(boundary)) {
      boundary <- x[grDevices::chull(x[predictors]), predictors]
    }
  }
  res <- bind_rows(out)
  attr(res, "type") <- type
  attr(res, "predictors") <- predictors
  attr(res, "boundary") <- tibble(boundary)
  res
}


#' Plot a two-predictor response surface
#'
#' Plots the output of [nm_response2d()] as a heat map (one panel per model), with
#' the convex hull of the records (dashed) showing where the surface is supported
#' by data.
#'
#' @param response_surface tibble returned by [nm_response2d()].
#' @param boundary logical. Draw the convex hull of the records. Default `TRUE`.
#'
#' @return A ggplot object.
#' @export
nm_response2d_plot <- function(response_surface, boundary = TRUE) {
  suitability <- .x <- .y <- NULL
  d <- as.data.frame(response_surface)
  vars <- attr(response_surface, "predictors")
  if (is.null(vars)) vars <- setdiff(names(d), c("model", "suitability"))[1:2]
  d$.x <- d[[vars[1]]]
  d$.y <- d[[vars[2]]]
  type <- attr(response_surface, "type")

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .x, y = .y)) +
    ggplot2::geom_raster(ggplot2::aes(fill = suitability)) +
    ggplot2::scale_fill_viridis_c(option = "magma", limits = c(0, 1), name = "Suitability") +
    ggplot2::facet_wrap(~model) +
    ggplot2::labs(x = vars[1], y = vars[2],
                  subtitle = if (identical(type, "partial")) "Partial dependence" else
                    "Marginal response (other predictors at their mean)") +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))
  hull <- attr(response_surface, "boundary")
  if (boundary && !is.null(hull) && nrow(hull) > 2) {
    hull <- data.frame(.x = hull[[vars[1]]], .y = hull[[vars[2]]])
    hull <- rbind(hull, hull[1, ])
    p <- p + ggplot2::geom_path(data = hull, inherit.aes = FALSE,
                                ggplot2::aes(x = .x, y = .y),
                                colour = "white", linetype = "dashed", linewidth = 0.7) +
      ggplot2::labs(caption = "Dashed: range of the records (convex hull)")
  }
  p
}
