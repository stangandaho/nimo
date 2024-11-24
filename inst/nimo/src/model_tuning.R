# Model Tuning
## Generalized Boosted Regression
gbm_tuning <- eventReactive(input$t_gbm, {
  req(input$t_gbm_predictors)
  tryCatch({
    tune_gbm(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$t_gbm_predictors,
      predictors_f = input$t_gbm_predictors_f,
      fit_formula = stats::formula(as.formula(input$t_gbm_fit_formula)),
      partition = ".part",
      thr = if(any(input$t_gbm_thr %in% c("sensitivity"))) {
        c(input$t_gbm_thr, "sens" = as.character(input$t_gbm_sens))
      } else{input$t_gbm_thr},
      grid =   expand.grid(
        n.trees = as.numeric( strsplit(input$t_gbm_n_trees, split = ",")[[1]] ),
        shrinkage = as.numeric( strsplit(input$t_gbm_shrinkage, split = ",")[[1]] ),
        n.minobsinnode = as.numeric( strsplit(input$t_gbm_n_minobsinnode, split = ",")[[1]] )
      ),
      metric = input$t_gbm_metric,
      n_cores = input$t_gbm_n_cores
    )
  }, error = error)
})

## Maximum Entropy Models
max_tuning <- eventReactive(input$t_max, {
  req(input$t_max_predictors)
  tryCatch({
    tune_max(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$t_max_predictors,
      predictors_f = input$t_max_predictors_f,
      partition = ".part",
      grid = expand.grid(
        regmult = as.numeric( strsplit(input$t_max_regmult, split = ",")[[1]] ),
        classes = input$t_max_classes
      ),
      thr = if(any(input$t_max_thr %in% c("sensitivity"))) {
        c(input$t_max_thr, "sens" = as.character(input$t_max_sens))
      } else{input$t_max_thr},
      clamp = input$t_max_clamp,
      pred_type = input$t_max_pred_type,
      metric = input$t_max_metric,
      n_cores = input$t_max_n_cores
    )
  }, error = error)
})

## Neural Networks Models
net_tuning <- eventReactive(input$t_net, {
  req(input$t_net_predictors)
  #tryCatch({
    tune_net(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$t_net_predictors,
      predictors_f = input$t_net_predictors_f,
      #fit_formula = input$t_net_formula,
      partition = ".part",
      thr = if(any(input$t_net_thr %in% c("sensitivity"))) {
        c(input$t_net_thr, "sens" = as.character(input$t_net_sens))
      } else{input$t_net_thr},
      grid = expand.grid(
        size = as.numeric( strsplit(input$t_net_size, split = ",")[[1]] ),
        decay = as.numeric( strsplit(input$t_net_decay, split = ",")[[1]] )
      ),
      metric = input$t_net_metric,
      n_cores = input$t_net_n_cores
    )
  #}, error = error)
})

## Random Forests Models
raf_tuning <- eventReactive(input$t_raf, {
  req(input$t_raf_predictors)
  tryCatch({
    tune_raf(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$t_raf_predictors,
      predictors_f = input$t_raf_predictors_f,
      fit_formula = stats::formula(as.formula(input$t_raf_formula)),
      partition = ".part",
      grid = expand.grid(mtry = as.numeric( strsplit(input$t_raf_mtry, split = ",")[[1]] ),
                         ntree = as.numeric( strsplit(input$t_raf_ntree, split = ",")[[1]] )),
      thr = if(any(input$t_raf_thr %in% c("sensitivity"))) {
        c(input$t_raf_thr, "sens" = as.character(input$t_raf_sens))
      } else{input$t_raf_thr},
      metric = input$t_raf_metric,
      n_cores = input$t_raf_n_cores
    )
  }, error = error)
})

## Support Vector Machine Models
svm_tuning <- eventReactive(input$t_svm, {
  req(input$t_svm_predictors)
  tryCatch({
    tune_svm(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$t_svm_predictors,
      predictors_f = input$t_svm_predictors_f,
      fit_formula = stats::formula(as.formula(input$t_svm_formula)),
      partition = ".part",
      thr = if(any(input$t_svm_thr %in% c("sensitivity"))) {
        c(input$t_svm_thr, "sens" = as.character(input$t_svm_sens))
      } else{input$t_svm_thr},
      grid = expand.grid(
        sigma =  as.numeric( strsplit(input$t_svm_sigma, split = ",")[[1]] ),
        C =  as.numeric( strsplit(input$t_svm_C, split = ",")[[1]] )
      ),
      metric = input$t_svm_metric,
      n_cores = input$t_svm_n_cores
    )
  }, error = error)
})

######
model_list <- reactiveValues( models = list() )

observeEvent(input$t_gbm, {
  showModal(models_modals("Generalized Boosted Regression Tuning output"))
  tryCatch({
    output$xx_model <- renderPrint(gbm_tuning()$model)
    output$xx_performance_metric <- render_dt(gbm_tuning()$performance)
    performance_metric <<- gbm_tuning()$performance
    output$xx_hyper_performance <- render_dt(gbm_tuning()$hyper_performance)
    output$xx_predicted_suitability <- DT::renderDT(gbm_tuning()$data_ens,
                                                    options = list(scrollX = TRUE))
    model_list$models$`Generalized Boosted Regression Tuned` <- gbm_tuning()
  }, error = error)
})


observeEvent(input$t_max, {
  showModal(models_modals("Maximum Entropy Tuning output"))
  tryCatch({
    output$xx_model <- renderPrint(max_tuning()$model)
    output$xx_performance_metric <- render_dt(max_tuning()$performance)
    performance_metric <<- max_tuning()$performance
    output$xx_hyper_performance <- render_dt(max_tuning()$hyper_performance)
    output$xx_predicted_suitability <- DT::renderDT(max_tuning()$data_ens,
                                                    options = list(scrollX = TRUE))
    model_list$models$`Maximum Entropy Tuned` <- max_tuning()
  }, error = error)
})

observeEvent(input$t_net, {
  showModal(models_modals("Neural Networks Tuning output"))
  tryCatch({
    output$xx_model <- renderPrint(net_tuning()$model)
    output$xx_performance_metric <- render_dt(net_tuning()$performance)
    performance_metric <<- net_tuning()$performance
    output$xx_hyper_performance <- render_dt(net_tuning()$hyper_performance)
    output$xx_predicted_suitability <- DT::renderDT(net_tuning()$data_ens,
                                                    options = list(scrollX = TRUE))
    model_list$models$`Neural Networks Tuned` <- net_tuning()
  }, error = error)
})

observeEvent(input$t_raf, {
  showModal(models_modals("Random Forests Tuning output"))
  tryCatch({
    output$xx_model <- renderPrint(raf_tuning()$model)
    output$xx_performance_metric <- render_dt(raf_tuning()$performance)
    performance_metric <<- raf_tuning()$performance
    output$xx_hyper_performance <- render_dt(raf_tuning()$hyper_performance)
    output$xx_predicted_suitability <- DT::renderDT(raf_tuning()$data_ens,
                                                    options = list(scrollX = TRUE))
    model_list$models$`Random Forests Tuned` <- raf_tuning()
  }, error = error)
})

observeEvent(input$t_svm, {
  showModal(models_modals("Support Vector Machine Tuning output"))
  tryCatch({
    output$xx_model <- renderPrint(svm_tuning()$model)
    output$xx_performance_metric <- render_dt(svm_tuning()$performance)
    performance_metric <<- svm_tuning()$performance
    output$xx_hyper_performance <- render_dt(svm_tuning()$hyper_performance)
    output$xx_predicted_suitability <- DT::renderDT(svm_tuning()$data_ens,
                                                    options = list(scrollX = TRUE))
    model_list$models$`Support Vector Machine Tuned` <- svm_tuning()
  }, error = error)
})

## models
models_names_df <- reactive({
  model_data <- data.frame(Model = names(model_list$models))
  return(model_data)
})


# Get the selected rows
sst <- reactive({
  input$fml_st_rows_selected
})
#all fitted models with for selection
output$fml_st <- DT::renderDataTable({
  datatable(models_names_df(), escape = FALSE,
            extensions = c("Buttons"), colnames = NULL,
            selection = "multiple", rownames = FALSE,
            options = list(dom = "t", paging = FALSE, ordering = FALSE,
                           buttons = list(
                             "select_all", "select_none", "deselect_all"
                           )))
})

# Selection for prediction
## Get the selected rows
st_selected <- reactive({
  input$st_fitted_model_list_dt_rows_selected
})
output$st_fitted_model_list_dt <- DT::renderDataTable({
  datatable(models_names_df(), escape = FALSE,
            extensions = c("Buttons"), colnames = NULL,
            selection = "multiple", rownames = FALSE,
            options = list(dom = "t", paging = FALSE, ordering = FALSE,
                           buttons = list(
                             "select_all", "select_none", "deselect_all"
                           )))
})

