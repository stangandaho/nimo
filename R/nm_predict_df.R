
#' Class used to dispatch predictions of a fitted model object
#' @noRd
nm_model_class <- function(m) {
  if (is.list(m) && !inherits(m, "nm_bioclim") && identical(names(m), "domain")) {
    return("domain")
  }
  gsub(".formula", "", tolower(class(m)[1]), fixed = TRUE)
}

#' Predict suitability of a fitted model (the `model` element of a fit_*, tune_*
#' or nm_fit_bioclim output) for the rows of a data.frame.
#' Uses the same prediction calls as nm_predict().
#' @noRd
nm_predict_df <- function(m, newdata, clamp = TRUE, pred_type = "cloglog") {
  newdata <- as.data.frame(newdata)
  out <- switch(
    nm_model_class(m),
    gam = mgcv::predict.gam(m, newdata, type = "response"),
    graf = predict.graf(object = m, newdata = newdata[, names(m$obsx), drop = FALSE],
                        type = "response", CI = NULL),
    glm = stats::predict(m, newdata, type = "response"),
    gbm = suppressMessages(stats::predict(m, newdata, type = "response")),
    maxnet = predict_maxnet(object = m, newdata = newdata, clamp = clamp, type = pred_type),
    nnet = stats::predict(m, newdata, type = "raw"),
    randomforest = stats::predict(m, newdata, type = "prob")[, 2],
    ksvm = kernlab::predict(m, newdata, type = "prob")[, 2],
    domain = flexsdm::map_env_dist(training_data = m$domain,
                                   projection_data = newdata[, names(m$domain), drop = FALSE],
                                   metric = "domain"),
    nm_bioclim = predict.nm_bioclim(m, newdata),
    stop("Model class not supported: ", class(m)[1])
  )
  as.numeric(out)
}
