# Spatial constraints, PCA predictors and truncation

## A priori constraint layers
msdm_layers <- reactiveVal(NULL)

## Principal components used as predictors
pca_used <- reactiveVal(NULL)
reset_pca_use <- function() {
  pca_used(NULL)
  forget_step("pca_use")
}
env_pred <- reactive({
  p <- pca_used()
  if (is.null(p)) env_base() else p
})

observeEvent(input$use_pca, {
  res <- tryCatch(reduce_colin(), error = function(e) NULL)
  if (is.null(res) || !inherits(res[[4]]$env_layer, "SpatRaster") || is.null(res[[4]]$cumulative_variance)) {
    showNotification("Run 'Reduce colinearity' with the PCA method first.", type = "warning", duration = 6)
    return()
  }
  pcs <- res[[4]]$env_layer
  pca_used(pcs)
  record_step("pca_use", "pca", "", c(
    "env <- colin$env_layer"
  ))
  showNotification(paste("Predictors replaced by", paste(names(pcs), collapse = ", ")),
                   duration = 6, type = "message")
})
observeEvent(input$unuse_pca, reset_pca_use())

output$pca_status <- renderText({
  p <- pca_used()
  if (is.null(p)) "Original predictors are used." else
    paste("Principal components used as predictors:", paste(names(p), collapse = ", "))
})

## Predictors used by every later step
env_layers <- reactive({
  m <- msdm_layers()
  if (is.null(m)) env_pred() else c(env_pred(), m)
})

msdm_methods <- c("Latitude and longitude" = "xy", "Distance to closest record" = "min",
                  "Cumulative distance to records" = "cml", "Kernel density of records" = "ker")

observeEvent(input$add_msdm, {
  req(occ_used(), env_pred())
  tryCatch({
    pres <- occ_used()
    pres <- pres[pres$pr_ab == 1, , drop = FALSE]
    out <- run_recorded(
      "msdm", "constraints", names(msdm_methods)[msdm_methods == input$msdm_method], "msdm",
      "flexsdm::msdm_priori", flexsdm::msdm_priori,
      list(data = nm_cv(pres, "occ[occ$pr_ab == 1, ]"),
           x = input$long_var,
           y = input$lat_var,
           method = input$msdm_method,
           env_layer = nm_cv(env_pred(), "env")),
      post = "env <- c(env, msdm)"
    )
    msdm_layers(out)
    showNotification(paste("Constraint layer added to the predictors:", paste(names(out), collapse = ", ")),
                     duration = 6, type = "message")
  }, error = error)
})

observeEvent(input$remove_msdm, {
  msdm_layers(NULL)
  forget_step("msdm")
})

output$msdm_status <- renderText({
  m <- msdm_layers()
  if (is.null(m)) "No constraint layer." else paste("Added:", paste(names(m), collapse = ", "))
})

## Truncation by extrapolation ----
observe({
  req(is.list(pred_rst()))
  updateSelectInput(session, "trunc_layer", choices = names(pred_rst()))
})

truncated <- eventReactive(input$truncate_pred, {
  req(input$trunc_layer, inherits(model_extrapo(), "SpatRaster"), is.list(pred_rst()))
  tryCatch({
    res <- run_recorded(
      "truncate", "extrapolation", paste("Truncation of", input$trunc_layer), "pred_truncated",
      "flexsdm::extra_truncate", flexsdm::extra_truncate,
      list(suit = nm_cv(pred_rst()[[input$trunc_layer]], pred_layer_code()[[input$trunc_layer]]),
           extra = nm_cv(model_extrapo(), "extra"),
           threshold = input$trunc_threshold,
           trunc_value = input$trunc_value)
    )
    res[[1]]
  }, error = error)
})
observeEvent(input$truncate_pred, updateTabsetPanel(session, "extra_tabs", selected = "Truncated prediction"))
observeEvent(input$extrapo_model, updateTabsetPanel(session, "extra_tabs", selected = "Extrapolation"))

output$trunc_raster <- renderPlot({
  req(inherits(truncated(), "SpatRaster"))
  terra::plot(truncated(), main = paste(input$trunc_layer, "- truncated where extrapolation >",
                                        input$trunc_threshold))
})

output$download_trunc <- downloadHandler(
  filename = function() paste0(gsub("[^A-Za-z0-9_-]+", "_", input$trunc_layer), "_truncated.tif"),
  content = function(file) terra::writeRaster(truncated(), file, overwrite = TRUE)
)
