# Response curves

observe({
  fitted <- names(model_list$models)
  keep <- intersect(isolate(input$rc_models), fitted)
  updateSelectInput(session, "rc_models", choices = fitted,
                    selected = if (length(keep) > 0) keep else fitted)
})

## Predictors of the selected models
observe({
  req(input$rc_models)
  vars <- unique(unlist(lapply(model_list$models[input$rc_models], function(m) {
    v <- unname(unlist(m$predictors))
    v[!is.na(v)]
  })))
  updateSelectInput(session, "rc_predictors", choices = vars,
                    selected = intersect(isolate(input$rc_predictors), vars))
})

resp_curves <- eventReactive(input$compute_rc, {
  req(input$rc_models, ready_df_mod())
  tryCatch({
    selected <- input$rc_models
    set.seed(global_seed())
    withProgress(message = "Computing response curves", value = 0.3, {
      run_recorded(
        "response", "response", "", "rc", "nimo::nm_response", nm_response,
        list(models = nm_cv(model_list$models[selected], models_code(selected)),
             data = nm_cv(ready_df_mod(), "sdm_data"),
             predictors = if (length(input$rc_predictors) > 0) input$rc_predictors else NULL,
             type = input$rc_type,
             resolution = input$rc_resolution),
        pre = "set.seed(seed)",
        post = paste0("nimo::nm_response_plot(rc", if (isTRUE(input$rc_rug)) ", data = sdm_data", ")")
      )
    })
  }, error = error)
})

output$rc_plot <- renderPlot({
  req(is.data.frame(resp_curves()))
  nm_response_plot(resp_curves(), data = if (isTRUE(input$rc_rug)) ready_df_mod() else NULL) +
    ggplot2::theme(text = ggplot2::element_text(size = 13))
})

output$download_rc <- downloadHandler(
  filename = function() "response_curves.csv",
  content = function(file) write.csv(resp_curves(), file, row.names = FALSE),
  contentType = "text/csv"
)

## Two-predictor surface ----
observe({
  req(input$rc_models)
  vars <- unique(unlist(lapply(model_list$models[input$rc_models], function(m) {
    v <- unname(unlist(m$predictors))
    v[!is.na(v)]
  })))
  vars <- vars[vapply(vars, function(v) is.numeric(ready_df_mod()[[v]]), logical(1))]
  updateSelectInput(session, "rs_x", choices = vars, selected = vars[1])
  updateSelectInput(session, "rs_y", choices = vars, selected = vars[min(2, length(vars))])
})

resp_surface <- eventReactive(input$compute_rs, {
  req(input$rc_models, input$rs_x, input$rs_y, ready_df_mod())
  tryCatch({
    if (input$rs_x == input$rs_y) stop("Choose two different predictors")
    uses <- vapply(model_list$models[input$rc_models], function(m) {
      all(c(input$rs_x, input$rs_y) %in% unlist(m$predictors))
    }, logical(1))
    selected <- input$rc_models[uses]
    if (length(selected) == 0) stop("None of the selected models uses both predictors")
    set.seed(global_seed())
    withProgress(message = "Computing response surface", value = 0.3, {
      run_recorded(
        "response2d", "response", "Surface of two predictors", "rs", "nimo::nm_response2d",
        nm_response2d,
        list(models = nm_cv(model_list$models[selected], models_code(selected)),
             data = nm_cv(ready_df_mod(), "sdm_data"),
             predictors = c(input$rs_x, input$rs_y),
             type = input$rc_type,
             resolution = input$rc_resolution),
        pre = "set.seed(seed)",
        post = "nimo::nm_response2d_plot(rs)"
      )
    })
  }, error = error)
})
observeEvent(input$compute_rs, updateTabsetPanel(session, "rc_tabs", selected = "Surface (two predictors)"))
observeEvent(input$compute_rc, updateTabsetPanel(session, "rc_tabs", selected = "Curves"))

output$rs_plot <- renderPlot({
  req(is.data.frame(resp_surface()))
  nm_response2d_plot(resp_surface()) + ggplot2::theme(text = ggplot2::element_text(size = 13))
})

output$download_rs <- downloadHandler(
  filename = function() "response_surface.csv",
  content = function(file) write.csv(resp_surface(), file, row.names = FALSE),
  contentType = "text/csv"
)
