# Extrapolation in environmental space and uncertainty

## Extrapolation in environmental space ----
observe({
  req(env_layers(), ready_df_mod())
  vars <- intersect(names(env_layers()), names(ready_df_mod()))
  updateSelectInput(session, "pe_x", choices = vars, selected = vars[1])
  updateSelectInput(session, "pe_y", choices = vars, selected = vars[min(2, length(vars))])
})

model_coords <- function() {
  xy <- if (spatial_partition()) c("x", "y") else c(input$long_var, input$lat_var)
  if (!all(xy %in% names(ready_df_mod()))) {
    stop("Coordinate columns (", paste(xy, collapse = ", "), ") not found in the modelling data")
  }
  xy
}

p_extra_res <- eventReactive(input$plot_p_extra, {
  req(inherits(model_extrapo(), "SpatRaster"), input$pe_x, input$pe_y)
  tryCatch({
    if (input$pe_x == input$pe_y) stop("Choose two different predictors")
    xy <- model_coords()
    # p_extra() needs coordinates named x and y
    train <- ready_df_mod()
    train_code <- "sdm_data"
    if (!identical(xy, c("x", "y"))) {
      train <- dplyr::rename(train, x = dplyr::all_of(xy[1]), y = dplyr::all_of(xy[2]))
      train_code <- paste0("dplyr::rename(sdm_data, x = ", nm_lit(xy[1]), ", y = ", nm_lit(xy[2]), ")")
    }
    set.seed(global_seed())
    run_recorded(
      "p_extra", "extrapolation", "Extrapolation in environmental and geographical space", "p_ext",
      "flexsdm::p_extra", flexsdm::p_extra,
      list(training_data = nm_cv(train, train_code),
           x = "x",
           y = "y",
           pr_ab = "pr_ab",
           extra_suit_data = nm_cv(model_extrapo(), "extra"),
           projection_data = kept_env(),
           predictors = c(input$pe_x, input$pe_y),
           geo_space = isTRUE(input$pe_geo)),
      pre = "set.seed(seed)",
      post = "p_ext"
    )
  }, error = error)
})
observeEvent(input$plot_p_extra, updateTabsetPanel(session, "extra_tabs", selected = "Environmental space"))

output$p_extra_plot <- renderPlot({
  req(inherits(p_extra_res(), "ggplot"))
  print(p_extra_res())
})

## Uncertainty ----
observe({
  fitted <- names(model_list$models)
  updateSelectInput(session, "unc_model", choices = fitted,
                    selected = intersect(isolate(input$unc_model), fitted))
})

uncertainty_res <- eventReactive(input$compute_unc, {
  req(input$unc_model, env_layers(), ready_df_mod())
  tryCatch({
    v <- model_var(input$unc_model)
    withProgress(message = paste("Bootstrapping", input$unc_model), value = 0.3, {
      run_recorded(
        paste0("uncertainty:", v), "uncertainty", input$unc_model, paste0(v, "_uncertainty"),
        "nimo::nm_uncertainty", nm_uncertainty,
        list(models = nm_cv(model_list$models[[input$unc_model]], v),
             training_data = nm_cv(ready_df_mod(), "sdm_data"),
             response = "pr_ab",
             projection_data = nm_cv(env_layers(), "env"),
             iteration = input$unc_iter,
             n_cores = input$unc_cores,
             background = nm_cv(ready_df_mod(), "sdm_data"))
      )
    })
  }, error = error)
})
observeEvent(input$compute_unc, updateTabsetPanel(session, "extra_tabs", selected = "Uncertainty"))

output$unc_raster <- renderPlot({
  req(inherits(uncertainty_res(), "SpatRaster"))
  terra::plot(uncertainty_res(),
              main = paste(input$unc_model, "- SD of", input$unc_iter, "bootstrap predictions"))
})

output$download_unc <- downloadHandler(
  filename = function() paste0(model_var(input$unc_model), "_uncertainty.tif"),
  content = function(file) terra::writeRaster(uncertainty_res(), file, overwrite = TRUE)
)
