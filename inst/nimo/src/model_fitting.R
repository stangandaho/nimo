# Model fitting
## Generalized Additive Models
gam_fitting <- eventReactive(input$fit_gam, {
    req(input$gam_predictors)
  tryCatch({
    fit_gam(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$gam_predictors ,
      predictors_f = input$gam_predictors_f,
      select_pred = input$gam_select_pred,
      partition = ".part",#input$gam_partition,
      thr = if(any(input$gam_thr %in% c("sensitivity"))) {
        c(input$gam_thr, "sens" = as.character(input$gam_sens))
      } else{input$gam_thr},
      fit_formula = stats::formula(as.formula(input$gam_fit_formula)),
      k = input$gam_k
    )
  }, error = error)
})

## Gaussian Process Models
gau_fitting <- eventReactive(input$fit_gau, {
    req(input$gau_predictors)
  tryCatch({
    fit_gau(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$gau_predictors ,
      predictors_f = input$gau_predictors_f,
      partition = ".part",#input$gau_partition,
      thr = if(any(input$gau_thr %in% c("sensitivity"))) {
        c(input$gau_thr, "sens" = as.character(input$gau_sens))
      } else{input$gau_thr}
    )
  }, error = error)
})

## Generalized Boosted Regression
gbm_fitting <- eventReactive(input$fit_gbm, {
    req(input$gbm_predictors)
  tryCatch({
    fit_gbm(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$gbm_predictors,
      predictors_f = input$gbm_predictors_f,
      fit_formula = stats::formula(as.formula(input$gbm_fit_formula)),
      partition = ".part",
      thr = if(any(input$gbm_thr %in% c("sensitivity"))) {
        c(input$gbm_thr, "sens" = as.character(input$gbm_sens))
      } else{input$gbm_thr},
      n_trees = input$gbm_n_trees,
      #n_minobsinnode = as.integer(nrow(data) * 0.5/4),
      shrinkage = input$gbm_shrinkage
    )
  }, error = error)
})

## Generalized Linear Models
glm_fitting <- eventReactive(input$fit_glm, {
      req(input$glm_predictors)
  tryCatch({
    fit_glm(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$glm_predictors,
      predictors_f = input$glm_predictors_f,
      select_pred = input$glm_select_pred,
      partition = ".part",
      thr = if(any(input$glm_thr %in% c("sensitivity"))) {
        c(input$glm_thr, "sens" = as.character(input$glm_sens))
      } else{input$glm_thr},
      fit_formula = stats::formula(as.formula(input$glm_fit_formula)),
      poly = input$glm_poly,
      inter_order = input$glm_inter_order
    )
  }, error = error)
  })

## Maximum Entropy Models
max_fitting <- eventReactive(input$fit_max, {
  req(input$max_predictors)
  tryCatch({
    fit_max(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$max_predictors,
      predictors_f = input$max_predictors_f,
      #fit_formula = stats::formula(as.formula(input$max_fit_formula)),
      partition = ".part",
      background = ready_df_mod(),
      thr = if(any(input$max_thr %in% c("sensitivity"))) {
        c(input$max_thr, "sens" = as.character(input$max_sens))
      } else{input$max_thr},
      clamp = input$max_clamp,
      classes = input$max_classes,
      pred_type = input$max_pred_type,
      regmult = input$max_regmult
    )
  }, error = error)
})

## Neural Networks Models
net_fitting <- eventReactive(input$fit_net, {
  req(input$net_predictors)
  tryCatch({
    fit_net(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$net_predictors,
      predictors_f = input$net_predictors_f,
      #fit_formula = stats::formula(as.formula(input$net_fit_formula)),
      partition = ".part",
      thr = if(any(input$net_thr %in% c("sensitivity"))) {
        c(input$net_thr, "sens" = as.character(input$net_sens))
      } else{input$net_thr},
      size = input$net_size,
      decay = input$net_decay
    )
  }, error = error)
})

## Random Forests Models
raf_fitting <- eventReactive(input$fit_raf, {
  req(input$raf_predictors)
  tryCatch({
    fit_raf(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$raf_predictors,
      predictors_f = input$raf_predictors_f,
      fit_formula = stats::formula(as.formula(input$raf_fit_formula)),
      ntree = input$raf_ntree,
      mtry = input$raf_mtry,
      partition = ".part",
      thr = if(any(input$raf_thr %in% c("sensitivity"))) {
        c(input$raf_thr, "sens" = as.character(input$raf_sens))
      } else{input$raf_thr}
    )
  }, error = error)
})

## Support Vector Machine Models
svm_fitting <- eventReactive(input$fit_svm, {
  req(input$svm_predictors)
  tryCatch({
    fit_svm(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$svm_predictors,
      predictors_f = input$svm_predictors_f,
      #fit_formula = stats::formula(as.formula(input$svm_fit_formula)),
      partition = ".part",
      thr = if(any(input$svm_thr %in% c("sensitivity"))) {
        c(input$svm_thr, "sens" = as.character(input$svm_sens))
      } else{input$svm_thr},
      sigma = "automatic", #input$svm_sigma,
      C = input$svm_C
    )
  }, error = error)
})

## Bioclim
bioc_fitting <- eventReactive(input$fit_bioc, {
  req(input$bioc_predictors)
  tryCatch({
    nm_fit_bioclim(
      data = ready_df_mod(),
      response = input$bioc_response,
      predictors = input$bioc_predictors,
      partition = ".part",
      thr = if(any(input$bioc_thr %in% c("sensitivity"))) {
        c(input$bioc_thr, "sens" = as.character(input$bioc_sens))
      } else{input$bioc_thr}
    )
  }, error = error)
})

######
model_list <- reactiveValues( models = list() )

observeEvent(input$fit_gam, {
  tryCatch({
    showModal(models_modals("Generalized Additive Models output"))
    output$xx_model <- renderPrint(gam_fitting()$model)
    output$xx_performance_metric <- render_dt(gam_fitting()$performance)
    performance_metric <<- gam_fitting()$performance
    output$xx_predicted_suitability <- DT::renderDT(gam_fitting()$data_ens,
                                                    options = list(scrollX = TRUE))
    model_list$models$`Generalized Additive Models` <- gam_fitting()
  }, error = error)
})

observeEvent(input$fit_gau, {
  tryCatch({
    showModal(models_modals("Gaussian Process Models output"))
    output$xx_model <- renderPrint(gau_fitting()$model)
    output$xx_performance_metric <- render_dt(gau_fitting()$performance)
    performance_metric <<- gau_fitting()$performance
    output$xx_predicted_suitability <- DT::renderDT(gau_fitting()$data_ens,
                                                    options = list(scrollX = TRUE))
    model_list$models$`Gaussian Process Models` <- gau_fitting()
  }, error = error)
})

observeEvent(input$fit_gbm, {
  tryCatch({
    showModal(models_modals("Generalized Boosted Regression Models output"))
    output$xx_model <- renderPrint(gbm_fitting()$model)
    output$xx_performance_metric <- render_dt(gbm_fitting()$performance)
    performance_metric <<- gbm_fitting()$performance
    output$xx_predicted_suitability <- DT::renderDT(gbm_fitting()$data_ens,
                                                    options = list(scrollX = TRUE))
    model_list$models$`Generalized Boosted Regression Models` <- gbm_fitting()
  }, error = error)
})

observeEvent(input$fit_glm, {
  tryCatch({
    showModal(models_modals("Generalized Linear Models output"))
    output$xx_model <- renderPrint(glm_fitting()$model)
    output$xx_performance_metric <- render_dt(glm_fitting()$performance)
    performance_metric <<- glm_fitting()$performance
    output$xx_predicted_suitability <- DT::renderDT(glm_fitting()$data_ens,
                                                    options = list(scrollX = TRUE))
    model_list$models$`Generalized Linear Models` <- glm_fitting()
  }, error = error)
})

observeEvent(input$fit_max, {
  tryCatch({
    showModal(models_modals("Maximum Entropy Models output"))
    output$xx_model <- renderPrint(max_fitting()$model)
    output$xx_performance_metric <- render_dt(max_fitting()$performance)
    performance_metric <<- max_fitting()$performance
    output$xx_predicted_suitability <- DT::renderDT(max_fitting()$data_ens,
                                                    options = list(scrollX = TRUE))
    model_list$models$`Maximum Entropy Models` <- max_fitting()
  }, error = error)
})

observeEvent(input$fit_net, {
  tryCatch({
    showModal(models_modals("Neural Networks Models output"))
    output$xx_model <- renderPrint(net_fitting()$model)
    output$xx_performance_metric <- render_dt(net_fitting()$performance)
    performance_metric <<- net_fitting()$performance
    output$xx_predicted_suitability <- DT::renderDT(net_fitting()$data_ens,
                                                    options = list(scrollX = TRUE))
    model_list$models$`Neural Networks Models` <- net_fitting()
  }, error = error)
})

observeEvent(input$fit_raf, {
  tryCatch({
    showModal(models_modals("Random Forests Models output"))
    output$xx_model <- renderPrint(raf_fitting()$model)
    output$xx_performance_metric <- render_dt(raf_fitting()$performance)
    performance_metric <<- raf_fitting()$performance
    output$xx_predicted_suitability <- DT::renderDT(raf_fitting()$data_ens,
                                                    options = list(scrollX = TRUE))
    model_list$models$`Random Forests Models` <- raf_fitting()
  }, error = error)
})

observeEvent(input$fit_svm, {
tryCatch({
  showModal(models_modals("Support Vector Machine Models output"))
  output$xx_model <- renderPrint(svm_fitting()$model)
  output$xx_performance_metric <- render_dt(svm_fitting()$performance)
  performance_metric <<- svm_fitting()$performance
  output$xx_predicted_suitability <- DT::renderDT(svm_fitting()$data_ens,
                                                  options = list(scrollX = TRUE))
  model_list$models$`Support Vector Machine Models` <- svm_fitting()
}, error = error)
})

observeEvent(input$fit_bioc, {
  tryCatch({
    showModal(models_modals("Bioclim Models output"))
    output$xx_model <- renderPrint(bioc_fitting()$model)
    output$xx_performance_metric <- render_dt(bioc_fitting()[[3]])
    performance_metric <<- bioc_fitting()$performance
    output$xx_predicted_suitability <- DT::renderDT(bioc_fitting()$data_ens,
                                                    options = list(scrollX = TRUE))
    model_list$models$`Bioclim Models` <- bioc_fitting()
  }, error = error)
})

#--------------------------------------------------------------#
## Models name in dataframe
models_names_df <- reactive({
  model_data <- data.frame(Model = names(model_list$models))
  return(model_data)
})


# Get the selected rows (for ensemble by mean, median, etc.)
sst <- reactive({
  input$fml_st_rows_selected
})
# All fitted models for selection
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

