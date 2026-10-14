#' Spatial uncertainty of a model by bootstrap
#'
#' Refits a model on bootstrap samples of the training data (80% of the records,
#' `iteration` times), predicts each refit over the study area and maps the
#' standard deviation of the predictions. High values show where the prediction
#' depends strongly on which records were used.
#'
#' Extends [flexsdm::sdm_uncertainty()] (used for most algorithms) with
#' [nm_fit_bioclim()] models and `fit_gbm()` models, using the same bootstrap
#' scheme (seed `ii` and an 80% bootstrap sample at iteration `ii`). Only the
#' predictors of the model are used, and cells where any of them is missing are
#' `NA`.
#'
#' @param models A single fitted model (output of a `fit_*`, `tune_*` or
#' [nm_fit_bioclim()] function).
#' @param training_data data.frame used to fit the model.
#' @param response character. Column with presences (1) and absences (0).
#' @param projection_data SpatRaster with the predictors.
#' @param iteration integer. Number of bootstrap refits. Default 50.
#' @param n_cores integer. Cores used by [flexsdm::sdm_uncertainty()]. Default 1.
#' @param background data.frame. Background points for Maxent and Gaussian
#' process models (see [flexsdm::sdm_uncertainty()]).
#' @param clamp,pred_type Passed to Maxent predictions.
#'
#' @return A SpatRaster named `uncertainty` (standard deviation of the bootstrap
#' predictions).
#'
#' @examples
#' \donttest{
#' library(terra)
#' df <- read.csv(system.file("extdata", "Gyps_africanus_KNP.csv", package = "nimo"))
#' env <- rast(system.file("extdata", "env_layers", c("dnw.tif", "dsm.tif"), package = "nimo"))
#' m <- nm_fit_bioclim(df, "pr_ab", c("dnw", "dsm"), ".part")
#' plot(nm_uncertainty(m, df, projection_data = env, iteration = 10))
#' }
#'
#' @export
nm_uncertainty <- function(models,
                           training_data,
                           response = "pr_ab",
                           projection_data,
                           iteration = 50,
                           n_cores = 1,
                           background = NULL,
                           clamp = TRUE,
                           pred_type = "cloglog") {
  if (is.null(models$model)) {
    stop("'models' must be a single fitted model (output of a fit_*, tune_* or nm_fit_bioclim function)")
  }
  if (iteration < 2) stop("'iteration' must be at least 2")
  m <- models$model
  vars <- unname(unlist(models$predictors))
  vars <- vars[!is.na(vars)]
  missing_vars <- setdiff(vars, names(projection_data))
  if (length(missing_vars) > 0) {
    stop("Predictor(s) not found in projection_data: ", paste(missing_vars, collapse = ", "))
  }
  proj <- projection_data[[vars]]
  training_data <- as.data.frame(training_data)
  cls <- nm_model_class(m)

  if (cls %in% c("nm_bioclim", "gbm")) {
    train <- training_data[, !startsWith(names(training_data), ".part"), drop = FALSE]
    cells <- terra::as.data.frame(proj, cells = TRUE, na.rm = TRUE)
    preds <- vapply(seq_len(iteration), function(ii) {
      set.seed(ii)
      db <- suppressWarnings(flexsdm::part_random(
        data = train, pr_ab = response,
        method = c(method = "boot", replicates = "1", proportion = "0.8")))
      db <- db[grepl("train", db$.part1), , drop = FALSE]
      refit <- if (cls == "nm_bioclim") {
        bioc_envelope(db[db[[response]] == 1, vars, drop = FALSE],
                      tails = m$tails, aggregation = m$aggregation)
      } else {
        pr_f <- models$predictors[, startsWith(names(models$predictors), "f"), drop = TRUE]
        pr_f <- if (length(pr_f) == 0) NULL else unname(unlist(pr_f))
        pr_c <- setdiff(vars, pr_f)
        suppressMessages(flexsdm::fit_gbm(
          data = db, response = response, predictors = pr_c, predictors_f = pr_f,
          partition = NULL, n_trees = m$n.trees, n_minobsinnode = m$n.minobsinnode,
          shrinkage = m$shrinkage
        ))$model
      }
      nm_predict_df(refit, cells[, vars, drop = FALSE], clamp = clamp, pred_type = pred_type)
    }, numeric(nrow(cells)))
    r <- proj[[1]]
    r[] <- NA
    r[cells$cell] <- apply(preds, 1, stats::sd)
  } else {
    r <- flexsdm::sdm_uncertainty(
      models = models, training_data = training_data, background = background,
      response = response, projection_data = proj, iteration = iteration,
      n_cores = n_cores, clamp = clamp, pred_type = pred_type
    )
  }
  r <- terra::mask(r, any(is.na(proj)), maskvalues = 1)
  names(r) <- "uncertainty"
  r
}
