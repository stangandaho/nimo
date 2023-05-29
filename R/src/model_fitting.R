## fitting
  gam_fitting <- eventReactive(input$fit_gam, {
      req(input$gam_predictors)
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
  })
  
  gau_fitting <- eventReactive(input$fit_gau, {
      req(input$gau_predictors)
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
  })
  
  gbm_fitting <- eventReactive(input$fit_gbm, {
      req(input$gbm_predictors)
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
  })
    
  glm_fitting <- eventReactive(input$fit_glm, {
        req(input$glm_predictors)
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
    })

  max_fitting <- eventReactive(input$fit_max, {
    req(input$max_predictors)
    classes <- paste0(input$max_classes, collapse = "")
    fit_max(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$max_predictors,
      predictors_f = input$max_predictors_f,
      #fit_formula = stats::formula(as.formula(input$max_fit_formula)),
      partition = ".part",
      thr = if(any(input$max_thr %in% c("sensitivity"))) { 
        c(input$max_thr, "sens" = as.character(input$max_sens))
      } else{input$max_thr},
      clamp = input$max_clamp,
      classes = classes,
      pred_type = input$max_pred_type,
      regmult = input$max_regmult
    )
  })
  
  net_fitting <- eventReactive(input$fit_net, {
    req(input$net_predictors)
    fit_net(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$net_predictors,
      predictors_f = input$net_predictors_f,
      fit_formula = stats::formula(as.formula(input$net_fit_formula)),
      partition = ".part",
      thr = if(any(input$net_thr %in% c("sensitivity"))) { 
        c(input$net_thr, "sens" = as.character(input$net_sens))
      } else{input$net_thr},
      size = input$net_size,
      decay = input$net_decay
    )
  })
  
  raf_fitting <- eventReactive(input$fit_raf, {
    req(input$raf_predictors)
    fit_raf(
      data = ready_df_mod(),
      response = "pr_ab",
      predictors = input$raf_predictors,
      predictors_f = input$raf_predictors_f,
      fit_formula = stats::formula(as.formula(input$raf_fit_formula)),
      partition = ".part",
      thr = if(any(input$raf_thr %in% c("sensitivity"))) { 
        c(input$raf_thr, "sens" = as.character(input$raf_sens))
      } else{input$raf_thr}
    )
  })
  
  svm_fitting <- eventReactive(input$fit_svm, {
    req(input$svm_predictors)
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
  })

######
model_list <- reactiveValues( models = list() )
  
observeEvent(input$fit_gam, {
  showModal(models_modals("Generalized Additive Models output"))
  output$xx_model <- renderPrint(gam_fitting()$model)
  output$xx_performance_metric <- render_dt(gam_fitting()$performance)
  output$xx_predicted_suitability <- DT::renderDT(gam_fitting()$data_ens, 
                                                  options = list(scrollX = TRUE))
  model_list$models$`Generalized Additive Models` <- gam_fitting()
})

observeEvent(input$fit_gau, {
  showModal(models_modals("Gaussian Process Models output"))
  output$xx_model <- renderPrint(gau_fitting()$model)
  output$xx_performance_metric <- render_dt(gau_fitting()$performance)
  output$xx_predicted_suitability <- DT::renderDT(gau_fitting()$data_ens, 
                                                  options = list(scrollX = TRUE))
  model_list$models$`Gaussian Process Models` <- gau_fitting()
})

observeEvent(input$fit_gbm, {
  showModal(models_modals("Generalized Boosted Regression Models output"))
  output$xx_model <- renderPrint(gbm_fitting()$model)
  output$xx_performance_metric <- render_dt(gbm_fitting()$performance)
  output$xx_predicted_suitability <- DT::renderDT(gbm_fitting()$data_ens, 
                                                  options = list(scrollX = TRUE))
  model_list$models$`Generalized Boosted Regression Models` <- gbm_fitting()
})

observeEvent(input$fit_glm, {
  showModal(models_modals("Generalized Linear Models output"))
  output$xx_model <- renderPrint(glm_fitting()$model)
  output$xx_performance_metric <- render_dt(glm_fitting()$performance)
  output$xx_predicted_suitability <- DT::renderDT(glm_fitting()$data_ens, 
                                                  options = list(scrollX = TRUE))
  model_list$models$`Generalized Linear Models` <- glm_fitting()
})

observeEvent(input$fit_max, {
  showModal(models_modals("Maximum Entropy Models output"))
  output$xx_model <- renderPrint(max_fitting()$model)
  output$xx_performance_metric <- render_dt(max_fitting()$performance)
  output$xx_predicted_suitability <- DT::renderDT(max_fitting()$data_ens, 
                                                  options = list(scrollX = TRUE))
  model_list$models$`Maximum Entropy Models` <- max_fitting()
})

observeEvent(input$fit_net, {
  showModal(models_modals("Neural Networks Models output"))
  output$xx_model <- renderPrint(net_fitting()$model)
  output$xx_performance_metric <- render_dt(net_fitting()$performance)
  output$xx_predicted_suitability <- DT::renderDT(net_fitting()$data_ens, 
                                                  options = list(scrollX = TRUE))
  model_list$models$`Neural Networks Models` <- net_fitting()
})

observeEvent(input$fit_raf, {
  showModal(models_modals("Random Forests Models output"))
  output$xx_model <- renderPrint(raf_fitting()$model)
  output$xx_performance_metric <- render_dt(raf_fitting()$performance)
  output$xx_predicted_suitability <- DT::renderDT(raf_fitting()$data_ens, 
                                                  options = list(scrollX = TRUE))
  model_list$models$`Random Forests Models` <- raf_fitting()
})

observeEvent(input$fit_svm, {
  showModal(models_modals("Support Vector Machine Models output"))
  output$xx_model <- renderPrint(svm_fitting()$model)
  output$xx_performance_metric <- render_dt(svm_fitting()$performance)
  output$xx_predicted_suitability <- DT::renderDT(svm_fitting()$data_ens, 
                                                  options = list(scrollX = TRUE))
  model_list$models$`Support Vector Machine Models` <- svm_fitting()
})

## models
models_names_df <- reactive({
  model_data <- data.frame(Model = names(model_list$models))
  return(model_data)
})
# Get the selected rows
selected <- reactive({
  input$fitted_model_list_dt_rows_selected
})
#all fitted models with for selection
output$fitted_model_list_dt <- DT::renderDataTable({
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

