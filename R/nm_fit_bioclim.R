#' Bioclim model
#'
#' Fit and validate a BIOCLIM climate-envelope model (Busby 1991; Booth et al. 2014).
#' BIOCLIM characterises the environmental envelope of a species from its
#' presence records only. For every predictor, the value of a site is located in the
#' empirical distribution of the presence values; sites near the median of
#' every predictor score close to 1 and sites outside the range observed at
#' presences score 0. Per-variable scores are then aggregated into a single
#' suitability value.
#'
#' The algorithm is implemented natively (no dependency on `dismo`). With the
#' default settings (`tails = "both"`, `aggregation = "min"`) predictions are
#' identical to `dismo::bioclim()`.
#'
#' @param data data.frame object with response (0,1) and predictors values.
#' Presences (1) are used to fit the envelope; absences, pseudo-absences or
#' background points (0) are only used to evaluate the model.
#' @param response character. Column name with species absence-presence data (0,1).
#' @param predictors character. Vector with the column names of quantitative predictor variables.
#' @param partition character. Column name with training and validation partition
#' groups (like part_* family output from `flexsdm`).
#' @param thr character. Threshold used to get binary suitability values (i.e. 0,1).
#' See [nm_eval()] for the available thresholds.
#' @param tails character. Which tail(s) of each predictor distribution limit
#' suitability. One of `"both"` (default), `"low"` (only low values are limiting,
#' values above the median score 1) or `"high"` (only high values are limiting).
#' Either a single value applied to all predictors, or a vector named by predictor.
#' @param aggregation character. How per-variable scores are combined into the
#' final suitability:
#' * `"min"` (default): the most limiting variable sets suitability (Liebig's
#' law of the minimum, classic BIOCLIM).
#' * `"gmean"`: geometric mean of per-variable scores. Still 0 outside the
#' envelope of any variable, but less dominated by a single variable.
#' * `"mean"`: arithmetic mean of per-variable scores, the most permissive.
#'
#' @return
#'
#' A list object with:
#' \itemize{
#' \item model: A `nm_bioclim` object holding the fitted envelope. It can be
#' used with [predict()][predict.nm_bioclim], [nm_predict()], [nm_bioclim_limiting()],
#' [nm_bioclim_envelope()] and [nm_bioclim_plot()].
#' \item predictors: A tibble with quantitative (c column names) variables use for modeling.
#' \item performance: Performance metric (see [nm_eval()]).
#' Threshold dependent metrics are calculated based on the threshold specified in the argument.
#' \item data_ens: Predicted suitability for each test partition.
#' }
#'
#' @references
#' Busby, J. R. (1991). BIOCLIM - a bioclimate analysis and prediction system.
#' Plant Protection Quarterly, 6, 8-9.
#'
#' Booth, T. H., Nix, H. A., Busby, J. R., & Hutchinson, M. F. (2014). BIOCLIM:
#' the first species distribution modelling package, its early applications and
#' relevance to most current MaxEnt studies. Diversity and Distributions, 20(1), 1-9.
#'
#' @examples
#'
#' wbv_path <- paste0(system.file("extdata", package = "nimo"), "/Gyps_africanus_KNP.csv")
#' wbv_df <- read.csv(wbv_path)
#' mod <- nm_fit_bioclim(data = wbv_df,
#'                       response = "pr_ab",
#'                       predictors = c("dnw", "dsm", "lst", "ndvi"),
#'                       partition = ".part",
#'                       thr = "max_sens_spec")
#' mod$model
#' mod$performance
#'
#' # Less strict aggregation, and only low NDVI values are limiting
#' mod2 <- nm_fit_bioclim(data = wbv_df,
#'                        response = "pr_ab",
#'                        predictors = c("dnw", "dsm", "lst", "ndvi"),
#'                        partition = ".part",
#'                        thr = "max_sens_spec",
#'                        tails = c(dnw = "both", dsm = "both", lst = "both", ndvi = "low"),
#'                        aggregation = "gmean")
#'
#' @importFrom dplyr bind_rows pull all_of starts_with filter select
#' across left_join relocate mutate tibble group_by summarise
#'
#' @importFrom stats sd complete.cases na.exclude na.omit
#' @importFrom flexsdm sdm_eval
#'
#' @export

nm_fit_bioclim <- function(data,
                           response,
                           predictors,
                           partition,
                           thr = NULL,
                           tails = "both",
                           aggregation = c("min", "gmean", "mean")) {
  . <- model <- TPR <- IMAE <- rnames <- thr_value <- n_presences <- n_absences <- threshold <- NULL
  aggregation <- match.arg(aggregation)
  data <- data.frame(data)

  # Check arguments
  if (missing(predictors) || length(predictors) == 0) {
    stop("At least one predictor must be provided in 'predictors'")
  }
  missing_cols <- setdiff(c(response, predictors), names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }
  if (!any(startsWith(names(data), partition))) {
    stop("No partition column starting with '", partition, "' found in data")
  }
  not_numeric <- predictors[!vapply(data[predictors], is.numeric, logical(1))]
  if (length(not_numeric) > 0) {
    stop("Bioclim only handles quantitative predictors. Not numeric: ",
         paste(not_numeric, collapse = ", "))
  }
  tails <- bioc_check_tails(tails, predictors)

  # Test response variable
  r_test <- (data %>% pull(response) %>% unique() %>% na.omit())
  if ((!all(r_test %in% c(0, 1)))) {
    stop("values of response variable do not match with 0 and 1")
  }
  if (!all(c(0, 1) %in% r_test)) {
    stop("Data must contain presences (1) and absences, pseudo-absences or background points (0).",
         " Presences are used to fit Bioclim and absences to evaluate it.")
  }

  variables <- bind_rows(c(c = predictors))

  data <- data %>%
    select(all_of(response), all_of(predictors), starts_with(partition))

  # Remove NAs
  complete_vec <- complete.cases(data[, c(response, unlist(variables))])
  if (sum(!complete_vec) > 0) {
    message(sum(!complete_vec), " rows were excluded from database because NAs were found")
    data <- data %>% filter(complete_vec)
  }
  rm(complete_vec)

  # Fit models
  np <- ncol(data %>% select(starts_with(partition)))
  p_names <- names(data %>% select(starts_with(partition)))
  eval_partial_list <- list()
  pred_test_ens <- vector("list", np)

  for (h in 1:np) {
    message("Replica number: ", h, "/", np)

    out <- bioc_train_test(data, p_names, h)
    train <- out$train
    test <- out$test
    np2 <- out$np2
    rm(out)

    eval_partial <- vector("list", np2)
    pred_test_ens[[h]] <- vector("list", np2)

    for (i in 1:np2) {
      message("Partition number: ", i, "/", np2)
      tryCatch({
        mod_i <- bioc_envelope(
          train[[i]][train[[i]][, response] == 1, predictors, drop = FALSE],
          tails = tails, aggregation = aggregation
        )

        pred_test <- data.frame(
          pr_ab = test[[i]][, response],
          pred = stats::predict(mod_i, test[[i]])
        )

        pred_test_ens[[h]][[i]] <- pred_test %>%
          mutate(rnames = rownames(test[[i]]))

        # Validation of model
        eval <- sdm_eval(
          p = pred_test$pred[pred_test$pr_ab == 1],
          a = pred_test$pred[pred_test$pr_ab == 0],
          thr = thr
        )

        eval_partial[[i]] <- tibble(model = "bioc", eval)
      }, error = function(e) {
        warning("Bioclim could not be evaluated for replica ", h, ", partition ", i,
                ": ", conditionMessage(e), call. = FALSE)
      })
    }
    names(eval_partial) <- seq_len(np2)
    names(pred_test_ens[[h]]) <- seq_len(np2)

    # Create final database with parameter performance
    eval_partial <-
      eval_partial[vapply(eval_partial, function(x) !is.null(dim(x)), logical(1))] %>%
      bind_rows(., .id = "partition")
    eval_partial_list[[h]] <- eval_partial
  }

  eval_partial <- eval_partial_list %>%
    bind_rows(., .id = "replica")
  if (nrow(eval_partial) == 0) {
    stop("Bioclim could not be evaluated on any partition. ",
         "Check that every test partition has presences and absences.")
  }

  eval_final <- eval_partial %>%
    group_by(model, threshold) %>%
    summarise(across(
      TPR:IMAE,
      list(mean = mean, sd = sd)
    ), .groups = "drop")

  # Bind data for ensemble
  names(pred_test_ens) <- seq_len(np)
  pred_test_ens <-
    lapply(pred_test_ens, function(x) {
      x <- x[vapply(x, function(y) !is.null(nrow(y)), logical(1))]
      bind_rows(x, .id = "part")
    }) %>%
    bind_rows(., .id = "replicates") %>%
    tibble() %>%
    relocate(rnames)

  # Fit final model with all presences
  mod <- bioc_envelope(data[data[, response] == 1, predictors, drop = FALSE],
                       tails = tails, aggregation = aggregation)

  pred_test <- data.frame(
    "pr_ab" = data[, response],
    "pred" = stats::predict(mod, data)
  )

  threshold <- sdm_eval(
    p = pred_test$pred[pred_test$pr_ab == 1],
    a = pred_test$pred[pred_test$pr_ab == 0],
    thr = thr
  )

  result <- list(
    model = mod,
    predictors = variables,
    performance = left_join(eval_final, threshold[1:4], by = "threshold") %>%
      relocate(model, threshold, thr_value, n_presences, n_absences),
    data_ens = pred_test_ens
  )
  return(result)
}


#' Predict suitability from a Bioclim model
#'
#' @param object A `nm_bioclim` object, i.e. the `model` element returned by
#' [nm_fit_bioclim()].
#' @param newdata data.frame, matrix or SpatRaster with (at least) the predictors used
#' to fit the model.
#' @param type character. `"suitability"` (default) returns the aggregated
#' suitability. `"partial"` returns the score of each predictor separately, which
#' shows how each variable contributes to (or limits) suitability.
#' @param ... Not used.
#'
#' @return A numeric vector (`type = "suitability"`) or a matrix with one column per
#' predictor (`type = "partial"`) when `newdata` is tabular; a SpatRaster when
#' `newdata` is a SpatRaster.
#'
#' @examples
#' wbv_df <- read.csv(system.file("extdata", "Gyps_africanus_KNP.csv", package = "nimo"))
#' mod <- nm_fit_bioclim(wbv_df, "pr_ab", c("dnw", "dsm", "lst", "ndvi"), ".part")
#' head(predict(mod$model, wbv_df))
#' head(predict(mod$model, wbv_df, type = "partial"))
#'
#' @importFrom stats predict
#' @export
predict.nm_bioclim <- function(object, newdata, type = c("suitability", "partial"), ...) {
  type <- match.arg(type)
  if (inherits(newdata, "SpatRaster")) {
    bioc_check_vars(object, names(newdata))
    out <- bioc_raster(newdata, object, function(p) {
      if (type == "partial") p else bioc_aggregate(object, p)
    })
    names(out) <- if (type == "partial") object$variables else "bioc"
    return(out)
  }
  newdata <- as.data.frame(newdata)
  bioc_check_vars(object, names(newdata))
  partial <- bioc_partial(object, as.matrix(newdata[, object$variables, drop = FALSE]))
  if (type == "partial") {
    return(partial)
  }
  bioc_aggregate(object, partial)
}


#' @export
print.nm_bioclim <- function(x, ...) {
  cat("Bioclim envelope model (nimo)\n")
  cat("  Presences    :", x$n, "\n")
  cat("  Aggregation  :", x$aggregation, "\n")
  cat("  Predictors   :", length(x$variables), "\n\n")
  env <- x$envelope
  num <- vapply(env, is.numeric, logical(1))
  env[num] <- lapply(env[num], signif, digits = 4)
  print(env, row.names = FALSE)
  invisible(x)
}


#' Map the variable that limits Bioclim suitability
#'
#' For every cell, identify the predictor with the lowest per-variable score,
#' i.e. the variable that sets suitability under Liebig's law of the minimum.
#' This is the classic BIOCLIM diagnostic map and explains *why* a site is
#' predicted unsuitable.
#'
#' @param model A `nm_bioclim` object or the list returned by [nm_fit_bioclim()].
#' @param env_layer SpatRaster with the predictors used to fit the model.
#'
#' @return A categorical SpatRaster whose levels are the predictor names.
#'
#' @examples
#' library(terra)
#' wbv_df <- read.csv(system.file("extdata", "Gyps_africanus_KNP.csv", package = "nimo"))
#' env <- rast(system.file("extdata", "env_layers", c("dnw.tif", "dsm.tif"), package = "nimo"))
#' mod <- nm_fit_bioclim(wbv_df, "pr_ab", c("dnw", "dsm"), ".part")
#' plot(nm_bioclim_limiting(mod, env))
#' plot(nm_bioclim_envelope(mod, env))
#'
#' @export
nm_bioclim_limiting <- function(model, env_layer) {
  model <- bioc_get_model(model)
  bioc_check_vars(model, names(env_layer))
  lim <- bioc_raster(env_layer, model, function(p) {
    max.col(-p, ties.method = "first")
  })
  levels(lim) <- data.frame(id = seq_along(model$variables), limiting = model$variables)
  names(lim) <- "limiting"
  lim
}


#' Classify sites into Bioclim envelope classes
#'
#' Classic discrete BIOCLIM classification (Nix 1986). A site is
#' * `core` when every predictor lies within the inner percentile range of the
#' presence values (by default 5th-95th percentiles),
#' * `marginal` when every predictor lies within the observed min-max range but at
#' least one is outside the core range,
#' * `outside` when at least one predictor is outside the observed range.
#'
#' @param model A `nm_bioclim` object or the list returned by [nm_fit_bioclim()].
#' @param env_layer SpatRaster or data.frame with the predictors used to fit the model.
#' @param core numeric. Lower and upper percentiles defining the core envelope.
#' Default `c(0.05, 0.95)`.
#'
#' @return A categorical SpatRaster (if `env_layer` is a SpatRaster) or a factor.
#'
#' @export
nm_bioclim_envelope <- function(model, env_layer, core = c(0.05, 0.95)) {
  model <- bioc_get_model(model)
  if (length(core) != 2 || any(core < 0) || any(core > 1) || core[1] >= core[2]) {
    stop("'core' must be two increasing values between 0 and 1")
  }
  bounds <- lapply(model$sorted, function(s) {
    c(min(s), stats::quantile(s, core, names = FALSE), max(s))
  })
  classify <- function(v) {
    in_range <- in_core <- matrix(TRUE, nrow(v), ncol(v))
    for (j in seq_along(model$variables)) {
      b <- bounds[[j]]
      in_range[, j] <- v[, j] >= b[1] & v[, j] <= b[4]
      in_core[, j] <- v[, j] >= b[2] & v[, j] <= b[3]
    }
    ifelse(rowSums(!in_range) > 0, 0, ifelse(rowSums(!in_core) > 0, 1, 2))
  }
  bioc_check_vars(model, names(env_layer))
  if (inherits(env_layer, "SpatRaster")) {
    out <- terra::predict(env_layer[[model$variables]], model, na.rm = TRUE,
                          fun = function(model, data, ...) classify(as.matrix(data)))
    levels(out) <- data.frame(id = 0:2, envelope = c("outside", "marginal", "core"))
    names(out) <- "envelope"
    return(out)
  }
  cls <- classify(as.matrix(as.data.frame(env_layer)[, model$variables, drop = FALSE]))
  factor(cls, levels = 0:2, labels = c("outside", "marginal", "core"))
}


#' Plot a Bioclim envelope for two predictors
#'
#' Draws the presences used to fit the model in a two-predictor space together with
#' the observed range (min-max, dashed) and the core envelope (inner percentiles,
#' filled) of each predictor. Optionally adds other points (e.g. absences or
#' background) to see how they fall relative to the envelope.
#'
#' @param model A `nm_bioclim` object or the list returned by [nm_fit_bioclim()].
#' @param x,y character. Predictors to plot. Default to the first two predictors.
#' @param data data.frame. Optional extra points to draw (must contain `x` and `y`).
#' @param core numeric. Lower and upper percentiles defining the core envelope.
#'
#' @return A ggplot object.
#'
#' @examples
#' wbv_df <- read.csv(system.file("extdata", "Gyps_africanus_KNP.csv", package = "nimo"))
#' mod <- nm_fit_bioclim(wbv_df, "pr_ab", c("dnw", "dsm", "lst", "ndvi"), ".part")
#' nm_bioclim_plot(mod, "lst", "ndvi", data = wbv_df[wbv_df$pr_ab == 0, ])
#'
#' @export
nm_bioclim_plot <- function(model, x = NULL, y = NULL, data = NULL, core = c(0.05, 0.95)) {
  model <- bioc_get_model(model)
  vars <- model$variables
  if (is.null(x)) x <- vars[1]
  if (is.null(y)) y <- vars[min(2, length(vars))]
  if (!all(c(x, y) %in% vars)) {
    stop("'x' and 'y' must be predictors of the model: ", paste(vars, collapse = ", "))
  }
  rng <- function(v) range(model$sorted[[v]])
  qnt <- function(v) stats::quantile(model$sorted[[v]], core, names = FALSE)
  pres <- data.frame(model$presence[, c(x, y)], type = "Presence (training)")
  names(pres)[1:2] <- c("x", "y")

  p <- ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = qnt(x)[1], xmax = qnt(x)[2],
                      ymin = qnt(y)[1], ymax = qnt(y)[2],
                      fill = "#065325", alpha = 0.15) +
    ggplot2::annotate("rect", xmin = rng(x)[1], xmax = rng(x)[2],
                      ymin = rng(y)[1], ymax = rng(y)[2],
                      fill = NA, colour = "#065325", linetype = "dashed")
  if (!is.null(data)) {
    other <- data.frame(x = data[[x]], y = data[[y]], type = "Other points")
    p <- p + ggplot2::geom_point(data = other, ggplot2::aes(x = x, y = y, colour = type),
                                 alpha = 0.5, size = 1.2)
  }
  p +
    ggplot2::geom_point(data = pres, ggplot2::aes(x = x, y = y, colour = type), size = 1.6) +
    ggplot2::scale_colour_manual(values = c("Presence (training)" = "#042d0f",
                                            "Other points" = "#B7C1C6"), name = NULL) +
    ggplot2::labs(x = x, y = y,
                  subtitle = sprintf("Shaded: core envelope (%g-%g%%) | dashed: observed range",
                                     core[1] * 100, core[2] * 100)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}


# Internal helpers ----

## Build a nm_bioclim envelope from a presence-only matrix/data.frame
bioc_envelope <- function(presence, tails = "both", aggregation = "min") {
  presence <- as.matrix(presence)
  vars <- colnames(presence)
  if (nrow(presence) < 2) {
    stop("At least two presences are needed to fit Bioclim")
  }
  tails <- bioc_check_tails(tails, vars)
  sorted <- lapply(vars, function(v) sort(presence[, v]))
  names(sorted) <- vars
  constant <- vars[vapply(sorted, function(s) length(unique(s)) == 1, logical(1))]
  if (length(constant) > 0) {
    warning("Predictor(s) with a single value at presences: ", paste(constant, collapse = ", "),
            ". Any other value scores 0.", call. = FALSE)
  }
  envelope <- data.frame(
    variable = vars,
    min = vapply(sorted, min, numeric(1)),
    q05 = vapply(sorted, stats::quantile, numeric(1), probs = 0.05, names = FALSE),
    median = vapply(sorted, stats::median, numeric(1)),
    q95 = vapply(sorted, stats::quantile, numeric(1), probs = 0.95, names = FALSE),
    max = vapply(sorted, max, numeric(1)),
    tails = unname(tails),
    row.names = NULL
  )
  structure(
    list(variables = vars, presence = presence, sorted = sorted, tails = tails,
         aggregation = aggregation, envelope = envelope, n = nrow(presence)),
    class = "nm_bioclim"
  )
}

## Per-variable percentile score (0-1)
bioc_partial <- function(object, x) {
  out <- matrix(NA_real_, nrow = nrow(x), ncol = length(object$variables),
                dimnames = list(NULL, object$variables))
  for (j in seq_along(object$variables)) {
    ref <- object$sorted[[j]]
    z <- x[, j]
    ok <- !is.na(z)
    below <- findInterval(z[ok], ref, left.open = TRUE) # n(ref < z)
    below_eq <- findInterval(z[ok], ref)                # n(ref <= z)
    r <- (below + 0.5 * (below_eq - below)) / length(ref)
    out[ok, j] <- switch(object$tails[[j]],
                         both = 2 * pmin(r, 1 - r),
                         low = ifelse(r > 0.5, 1, 2 * r),
                         high = ifelse(r < 0.5, 1, 2 * (1 - r)))
  }
  out
}

## Apply fun() to the partial scores of a SpatRaster
bioc_raster <- function(env_layer, model, fun) {
  terra::predict(env_layer[[model$variables]], model, na.rm = TRUE,
                 fun = function(model, data, ...) fun(bioc_partial(model, as.matrix(data))))
}

bioc_aggregate <- function(object, partial) {
  switch(object$aggregation,
         min = apply(partial, 1, min),
         mean = rowMeans(partial),
         gmean = exp(rowMeans(log(partial))))
}

bioc_check_tails <- function(tails, predictors) {
  if (!all(tails %in% c("both", "low", "high"))) {
    stop("'tails' values must be 'both', 'low' or 'high'")
  }
  if (length(tails) == 1 && is.null(names(tails))) {
    tails <- stats::setNames(rep(tails, length(predictors)), predictors)
  }
  if (is.null(names(tails)) && length(tails) == length(predictors)) {
    names(tails) <- predictors
  }
  if (!all(predictors %in% names(tails))) {
    stop("'tails' must be a single value or a vector named by predictor")
  }
  tails[predictors]
}

bioc_check_vars <- function(object, available) {
  miss <- setdiff(object$variables, available)
  if (length(miss) > 0) {
    stop("Predictor(s) used to fit Bioclim not found: ", paste(miss, collapse = ", "))
  }
}

bioc_get_model <- function(model) {
  if (is.list(model) && !inherits(model, "nm_bioclim") && inherits(model$model, "nm_bioclim")) {
    model <- model$model
  }
  if (!inherits(model, "nm_bioclim")) {
    stop("'model' must be fitted with nm_fit_bioclim()")
  }
  model
}

## Train/test split for a partition column (adapted from flexsdm)
bioc_train_test <- function(data, p_names, h) {
  train <- list()
  test <- list()
  others <- p_names[p_names != p_names[h]]
  keep <- setdiff(names(data), others)
  part <- data[, p_names[h]]

  if (any(c("train", "train-test", "test") %in% unique(part))) {
    np2 <- 1
    train[[1]] <- data[grepl("train", part), keep, drop = FALSE]
    test[[1]] <- data[grepl("test", part), keep, drop = FALSE]
  } else {
    np2 <- max(part)
    for (i in 1:np2) {
      train[[i]] <- data[part != i, keep, drop = FALSE]
      test[[i]] <- data[part == i, keep, drop = FALSE]
    }
  }
  list(train = train, test = test, np2 = np2)
}
