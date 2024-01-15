# Fitting of Ensemble of Small Models (ESM)
## Generalized Additive Models
gam_esm_fitting <- eventReactive(input$fit_gam_esm, {
  req(input$gam_esm_predictors)
  tryCatch({
    esm_gam(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$gam_esm_predictors ,
      partition = ".part",#input$gam_esm_partition,
      thr = if(any(input$gam_esm_thr %in% c("sensitivity"))) {
        c(input$gam_esm_thr, "sens" = as.character(input$gam_esm_sens))
      } else{input$gam_esm_thr},
      k = input$gam_esm_k
    )
  }, error = error)
})

## Gaussian Process Models
gau_esm_fitting <- eventReactive(input$fit_gau_esm, {
  req(input$gau_esm_predictors)
  tryCatch({
    esm_gau(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$gau_esm_predictors ,
      partition = ".part",#input$gau_esm_partition,
      thr = if(any(input$gau_esm_thr %in% c("sensitivity"))) {
        c(input$gau_esm_thr, "sens" = as.character(input$gau_esm_sens))
      } else{input$gau_esm_thr}
    )
  }, error = error)
})

## Generalized Boosted Regression
gbm_esm_fitting <- eventReactive(input$fit_gbm_esm, {
  req(input$gbm_esm_predictors)
  tryCatch({
    esm_gbm(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$gbm_esm_predictors,
      fit_formula = stats::formula(as.formula(input$gbm_esm_fit_formula)),
      partition = ".part",
      thr = if(any(input$gbm_esm_thr %in% c("sensitivity"))) {
        c(input$gbm_esm_thr, "sens" = as.character(input$gbm_esm_sens))
      } else{input$gbm_esm_thr},
      n_trees = input$gbm_esm_n_trees,
      #n_minobsinnode = as.integer(nrow(data) * 0.5/4),
      shrinkage = input$gbm_esm_shrinkage
    )
  }, error = error)
})

## Generalized Linear Models
glm_esm_fitting <- eventReactive(input$fit_glm_esm, {
  req(input$glm_esm_predictors)
  tryCatch({
    esm_glm(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$glm_esm_predictors,
      partition = ".part",
      thr = if(any(input$glm_esm_thr %in% c("sensitivity"))) {
        c(input$glm_esm_thr, "sens" = as.character(input$glm_esm_sens))
      } else{input$glm_esm_thr},
      poly = input$glm_esm_poly,
      inter_order = input$glm_esm_inter_order
    )
  }, error = error)
})

## Maximum Entropy Models
max_esm_fitting <- eventReactive(input$fit_max_esm, {
  req(input$max_esm_predictors)
  classes <- paste0(input$max_esm_classes, collapse = "")
  tryCatch({
    esm_max(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$max_esm_predictors,
      partition = ".part",
      thr = if(any(input$max_esm_thr %in% c("sensitivity"))) {
        c(input$max_esm_thr, "sens" = as.character(input$max_esm_sens))
      } else{input$max_esm_thr},
      clamp = input$max_esm_clamp,
      classes = classes,
      pred_type = input$max_esm_pred_type,
      regmult = input$max_esm_regmult
    )
  }, error = error)
})

## Neural Networks Models
net_esm_fitting <- eventReactive(input$fit_net_esm, {
  req(input$net_esm_predictors)
  tryCatch({
    esm_net(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$net_esm_predictors,
      partition = ".part",
      thr = if(any(input$net_esm_thr %in% c("sensitivity"))) {
        c(input$net_esm_thr, "sens" = as.character(input$net_esm_sens))
      } else{input$net_esm_thr},
      size = input$net_esm_size,
      decay = input$net_esm_decay
    )
  }, error = error)
})

## Support Vector Machine Models
svm_esm_fitting <- eventReactive(input$fit_svm_esm, {
  req(input$svm_esm_predictors)
  tryCatch({
    esm_svm(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$svm_esm_predictors,
      partition = ".part",
      thr = if(any(input$svm_esm_thr %in% c("sensitivity"))) {
        c(input$svm_esm_thr, "sens" = as.character(input$svm_esm_sens))
      } else{input$svm_esm_thr},
      sigma = "automatic", #input$svm_esm_sigma,
      C = input$svm_esm_C
    )
  }, error = error)
})

######
es_model_list <- reactiveValues( models = list() )
observeEvent(input$esm, {
  output$xx_predicted_suitability <- render_dt(
    datatable(data = data.frame(x = "No predicted suitability table for Ensembles of Small Models"),
              colnames = NULL, rownames = FALSE, options = list(scrollX = TRUE, paging = FALSE,
                                                              ordering = FALSE, searching = FALSE)))
  })

observeEvent(input$fit_gam_esm, {
  tryCatch({
    showModal(models_modals("Generalized Additive Models on ESM output"))
    output$xx_model <- renderPrint(gam_esm_fitting()$esm_model)
    output$xx_performance_metric <- render_dt(gam_esm_fitting()$performance)
    performance_metric <<- gam_esm_fitting()$performance
    es_model_list$models$`Generalized Additive Models - ESM` <- gam_esm_fitting()
  }, error = error)
})

observeEvent(input$fit_gau_esm, {
  tryCatch({
    showModal(models_modals("Gaussian Process Models on ESM output"))
    output$xx_model <- renderPrint(gau_esm_fitting()$esm_model)
    output$xx_performance_metric <- render_dt(gau_esm_fitting()$performance)
    performance_metric <<- gau_esm_fitting()$performance
    es_model_list$models$`Gaussian Process Models - ESM` <- gau_esm_fitting()
  }, error = error)
})

observeEvent(input$fit_gbm_esm, {
  tryCatch({
    showModal(models_modals("Generalized Boosted Regression Models on ESM output"))
    output$xx_model <- renderPrint(gbm_esm_fitting()$esm_model)
    output$xx_performance_metric <- render_dt(gbm_esm_fitting()$performance)
    performance_metric <<- gbm_esm_fitting()$performance
    es_model_list$models$`Generalized Boosted Regression Models - ESM` <- gbm_esm_fitting()
  }, error = error)
})

observeEvent(input$fit_glm_esm, {
  tryCatch({
    showModal(models_modals("Generalized Linear Models on ESM output"))
    output$xx_model <- renderPrint(glm_esm_fitting()$esm_model)
    output$xx_performance_metric <- render_dt(glm_esm_fitting()$performance)
    performance_metric <<- glm_esm_fitting()$performance
    es_model_list$models$`Generalized Linear Models - ESM` <- glm_esm_fitting()
  }, error = error)
})

observeEvent(input$fit_max_esm, {
  tryCatch({
    showModal(models_modals("Maximum Entropy Models on ESM output"))
    output$xx_model <- renderPrint(max_esm_fitting()$esm_model)
    output$xx_performance_metric <- render_dt(max_esm_fitting()$performance)
    performance_metric <<- max_esm_fitting()$performance
    es_model_list$models$`Maximum Entropy Models - ESM` <- max_esm_fitting()
  }, error = error)
})

observeEvent(input$fit_net_esm, {
  tryCatch({
    showModal(models_modals("Neural Networks Models on ESM output"))
    output$xx_model <- renderPrint(net_esm_fitting()$esm_model)
    output$xx_performance_metric <- render_dt(net_esm_fitting()$performance)
    performance_metric <<- net_esm_fitting()$performance
    es_model_list$models$`Neural Networks Models - ESM` <- net_esm_fitting()
  }, error = error)
})

observeEvent(input$fit_svm_esm, {
  tryCatch({
    showModal(models_modals("Support Vector Machine Models on ESM output"))
    output$xx_model <- renderPrint(svm_esm_fitting()$esm_model)
    output$xx_performance_metric <- render_dt(svm_esm_fitting()$performance)
    performance_metric <<- svm_esm_fitting()$performance
    es_model_list$models$`Support Vector Machine Models - ESM` <- svm_esm_fitting()
  }, error = error)
})


## Ensemble Small Models name in dataframe for selection
esm_models_names_df <- reactive({
  model_data <- data.frame(Model = names(es_model_list$models))
  return(model_data)
})


# Selection for prediction
## Get the selected rows
esm_selected <- reactive({
  input$esm_fitted_model_list_dt_rows_selected
})
output$esm_fitted_model_list_dt <- DT::renderDataTable({
  datatable(esm_models_names_df(), escape = FALSE,
            extensions = c("Buttons"), colnames = NULL,
            selection = "multiple", rownames = FALSE,
            options = list(dom = "t", paging = FALSE, ordering = FALSE,
                           buttons = list(
                             "select_all", "select_none", "deselect_all"
                           )))
})
