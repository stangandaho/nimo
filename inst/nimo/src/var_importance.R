# Variable importance

observe({
  fitted <- names(model_list$models)
  keep <- intersect(isolate(input$vi_models), fitted)
  updateSelectInput(session, "vi_models", choices = fitted,
                    selected = if (length(keep) > 0) keep else fitted)
})

var_imp <- eventReactive(input$compute_vi, {
  req(input$vi_models, input$vi_metrics, ready_df_mod())
  tryCatch({
    selected <- input$vi_models
    set.seed(global_seed())
    withProgress(message = "Computing variable importance", value = 0.3, {
      run_recorded(
        "varimp", "varimp", "", "vi", "nimo::nm_varimp", nm_varimp,
        list(models = nm_cv(model_list$models[selected], models_code(selected)),
             data = nm_cv(ready_df_mod(), "sdm_data"),
             response = "pr_ab",
             n_sim = input$vi_n_sim,
             metrics = input$vi_metrics,
             thr = input$vi_thr),
        pre = "set.seed(seed)",
        post = paste0("nimo::nm_varimp_plot(vi, relative = ", isTRUE(input$vi_relative), ")")
      )
    })
  }, error = error)
})

output$vi_plot <- renderPlot({
  req(is.data.frame(var_imp()))
  nm_varimp_plot(var_imp(), relative = isTRUE(input$vi_relative)) +
    ggplot2::theme(text = ggplot2::element_text(size = 13))
})

output$vi_table <- render_dt({
  req(is.data.frame(var_imp()))
  vi <- as.data.frame(var_imp())
  num <- vapply(vi, is.numeric, logical(1))
  vi[num] <- lapply(vi[num], round, digits = 4)
  vi
})

output$download_vi <- downloadHandler(
  filename = function() "variable_importance.csv",
  content = function(file) write.csv(var_imp(), file, row.names = FALSE),
  contentType = "text/csv"
)
