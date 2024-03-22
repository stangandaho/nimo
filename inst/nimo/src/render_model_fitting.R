thr <- c("No omission" = "lpt", "Sensitivity = specificity" = "equal_sens_spec",
         "TSS" = "max_sens_spec", "Jaccard" = "max_jaccard",
         "Sorensen" = "max_sorensen", "FPB" = "max_fpb", "Sensitivity" = "sensitivity")
# Metric
metric <- c("SORENSEN", "JACCARD", "FPB", "TSS", "KAPPA", "AUC", "IMAE", "BOYCE")

fit_model_btn_style <- paste0("background-color:", "#065325;", "color:#ffffff;")
fit_model_btn_icon = icon("buromobelexperte")

output$dynamic_model_fitting <- renderUI({
  selected_order <- input$fit_model_algorithm

  columns <- lapply(selected_order, function(order) {
    ## ESM
    if (order %in% c("gam") && input$esm == TRUE) {
      div(style = "flex: 1;  margin-right: 20px;",
          h4("Generalized Additive Models - ESM"),
          selectInput("gam_esm_predictors", "Quantitative predictors", choices = c(), multiple = T),
          selectInput("gam_esm_thr", "Threshold", choices = thr, multiple = T),
          conditionalPanel("input.gam_esm_thr.includes('sensitivity')",
                           numericInput("gam_esm_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
          numericInput("gam_esm_k", "Basis dimension", value = -1),
          actionButton("fit_gam_esm", "Fit GAM - ESM", icon = fit_model_btn_icon, style = fit_model_btn_style)
      )

    } else if (order  %in% c("gau") && input$esm == TRUE) {
      div(style = "flex: 1;margin-right: 20px;",
          h4("Gaussian Process Models - ESM"),
          selectInput("gau_esm_predictors", "Quantitative predictors", choices = c(), multiple = T),
          selectInput("gau_esm_thr", "Threshold", choices = thr, multiple = T),
          conditionalPanel("input.gau_esm_thr.includes('sensitivity')",
                           numericInput("gau_esm_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
          actionButton("fit_gam_esm", "Fit GPM - ESM", icon = fit_model_btn_icon, style = fit_model_btn_style))

    } else if (order  %in% c("gbm") && input$esm == TRUE) {
      div(style = "flex: 1;margin-right: 20px;",
          h4("Generalized Boosted Models - ESM"),
          selectInput("gbm_esm_predictors", "Quantitative predictors", choices = c(), multiple = T),
          selectInput("gbm_esm_thr", "Threshold", choices = thr, multiple = T),
          conditionalPanel("input.gbm_esm_thr.includes('sensitivity')",
                           numericInput("gbm_esm_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
          numericInput("gbm_esm_n_trees", label = "Number of trees", value = 100, min = 1),
          numericInput("gbm_esm_shrinkage", label = "Learning rate", value = 0.1, min = 0.001, max = 0.1, step = 0.001),
          actionButton("fit_gbm_esm", "Fit GBM - ESM", icon = fit_model_btn_icon, style = fit_model_btn_style))

    } else if (order  %in% c("glm") && input$esm == TRUE) {
      div(style = "flex: 1;margin-right: 20px;",
          h4("Generalized Linear Models - ESM"),
          selectInput("glm_esm_predictors", "Quantitative predictors", choices = c(), multiple = T),
          selectInput("glm_esm_thr", "Threshold", choices = thr, multiple = T),
          conditionalPanel("input.glm_esm_thr.includes('sensitivity')",
                           numericInput("glm_esm_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
          numericInput("glm_esm_poly", label = "Polynomials", value = 0, min = 1),
          numericInput("glm_esm_inter_order", label = "Interaction order", value = 0.1, min = 0.001, max = 0.1, step = 0.001),
          actionButton("fit_glm_esm", "Fit GLM - ESM", icon = fit_model_btn_icon, style = fit_model_btn_style))
    } else if (order  %in% c("max") && input$esm == TRUE) {
      div(style = "flex: 1;margin-right: 20px;",
          h4("Maximum Entropy Models - ESM"),
          selectInput("max_esm_predictors", "Quantitative predictors", choices = c(), multiple = T),
          selectInput("max_esm_clamp", "Clamp", choices = c("No" = FALSE, "Yes" = TRUE),
                      selected = FALSE),
          selectInput("max_esm_thr", "Threshold", choices = thr, multiple = T),
          conditionalPanel("input.max_esm_thr.includes('sensitivity')",
                           numericInput("max_esm_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
          #textInput("glm_fit_formula", "Model formula"),
          selectInput("max_esm_classes", label = "Classes", multiple = T,
                      c("Default" = "default", "Linear" = "l", "Quadratic" = "q", "Hinge" = "h", "Product" = "p", "Threshold" = "t")
          ),
          selectInput("max_esm_pred_type", label = "Type of response",
                      c("Link" = "link", "Exponential" = "exponential", "Cloglog" = "cloglog", "Logistic" = "logistic"),
                      selected = "Cloglog"),
          numericInput("max_esm_regmult", label = "Regularization", value = 2.5, min = 1),
          actionButton("fit_max_esm", "Fit MAX - ESM", icon = fit_model_btn_icon, style = fit_model_btn_style))
    } else if (order  %in% c("net") && input$esm == TRUE) {
      div(style = "flex: 1;margin-right: 20px;",
          h4("Neural Networks Models - ESM"),
          selectInput("net_esm_predictors", "Quantitative predictors", choices = c(), multiple = T),
          selectInput("net_esm_thr", "Threshold", choices = thr, multiple = T),
          conditionalPanel("input.net_esm_thr.includes('sensitivity')",
                           numericInput("net_esm_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
          numericInput("net_esm_size", label = "Number of units", value = 2, min = 0),
          numericInput("net_esm_decay", label = "Weight decay", value = 0.1, min = 0, step = 0.01),
          actionButton("fit_net_esm", "Fit NET - ESM", icon = fit_model_btn_icon, style = fit_model_btn_style))
    } else if (order  %in% c("raf") && input$esm == TRUE) {
      disable(
        div(style = "flex: 1; margin-right: 20px;",
            h4("Random Forests Models - ESM"),
            selectInput("raf_esm_predictors", "Quantitative predictors", choices = c(), multiple = T),
            selectInput("raf_esm_predictors_f", "Qualitative predictors", choices = c(), multiple = T),
            selectInput("raf_esm_thr", "Threshold", choices = thr, multiple = T),
            conditionalPanel("input.raf_esm_thr.includes('sensitivity')",
                             numericInput("raf_esm_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
            textInput("raf_esm_fit_formula", "Model formula"),
            actionButton("fit_raf_esm", "Fit RAF - ESM", icon = fit_model_btn_icon, style = fit_model_btn_style))
      )
    } else if (order  %in% c("svm") && input$esm == TRUE) {
      div(style = "flex: 1; margin-right: 20px;",
          h4("Support Vector Machine Models - ESM"),
          selectInput("svm_esm_predictors", "Quantitative predictors", choices = c(), multiple = T),
          selectInput("svm_esm_thr", "Threshold", choices = thr, multiple = T),
          conditionalPanel("input.svm_esm_thr.includes('sensitivity')",
                           numericInput("svm_esm_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
          #selectInput("svm_sigma", "Inverse kernel width", choices = c("automatic"), selected = "automatic"),
          numericInput("svm_esm_C", "Cost of constraints violation", value = 1, min = 1),
          actionButton("fit_svm_esm", "Fit SVM - ESM", icon = fit_model_btn_icon, style = fit_model_btn_style))
    }
    ## TUNING
    else if (order  %in% c("gbm") && input$tuning == TRUE) {
      div(style = "flex: 1;margin-right: 20px;",
          h4("Generalized Boosted - Tuning"),
          selectInput("t_gbm_predictors", "Quantitative predictors", choices = c(), multiple = T),
          selectInput("t_gbm_predictors_f", "Qualitative predictors", choices = c(), multiple = T),
          selectInput("t_gbm_thr", "Threshold", choices = thr, multiple = T),
          conditionalPanel("input.t_gbm_thr.includes('sensitivity')",
                           numericInput("t_gbm_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
          textInput("t_gbm_fit_formula", "Model formula"),
          # Grid Construction
          textInput("t_gbm_n_trees", label = "Number of trees",
                    value = "20, 50, 100", placeholder = "20, 50, 100"),
          textInput("t_gbm_shrinkage", label = "Learning rate",
                    value = "0.1, 0.5, 1", placeholder = "0.1, 0.5, 1"),
          textInput("t_gbm_n_minobsinnode", label = "Minimum leaf size",
                    value = "1, 4, 7, 9", placeholder = "1, 4, 7, 9"),
          selectInput("t_gbm_metric", "Metric", choices = metric, selected = "TSS"),
          numericInput(inputId = "t_gbm_n_cores", "Number of cores", min = 1, value = 1),
          actionButton("t_gbm", "Tune GBM", icon = fit_model_btn_icon, style = fit_model_btn_style))

    } else if (order  %in% c("max") && input$tuning == TRUE) {
      div(style = "flex: 1;margin-right: 20px;",
          h4("Maximum Entropy - Tuning"),
          selectInput("t_max_predictors", "Quantitative predictors", choices = c(), multiple = T),
          selectInput("t_max_predictors_f", "Qualitative predictors", choices = c(), multiple = T),
          selectInput("t_max_clamp", "Clamp", choices = c("No" = FALSE, "Yes" = TRUE),
                      selected = TRUE),
          selectInput("t_max_thr", "Threshold", choices = thr, multiple = T),
          conditionalPanel("input.t_max_thr.includes('sensitivity')",
                           numericInput("t_max_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
          # Grid Construction
          selectInput("t_max_classes", label = "Classes", multiple = T,
                      c("Default" = "default", "Linear" = "l", "Quadratic" = "q", "Hinge" = "h", "Product" = "p", "Threshold" = "t")
          ),
          textInput("t_max_regmult", label = "Regularization",
                    value = "0.1, 0.4, 0.5, 0.6", placeholder = "0.1, 0.4, 0.5, 0.6"),

          selectInput("t_max_pred_type", label = "Type of response",
                      c("Link" = "link", "Exponential" = "exponential", "Cloglog" = "cloglog", "Logistic" = "logistic"),
                      selected = "Cloglog"),
          selectInput("t_max_metric", "Metric", choices = metric, selected = "TSS"),
          numericInput(inputId = "t_max_n_cores", "Number of cores", min = 1, value = 1),
          actionButton("t_max", "Tune MAX", icon = fit_model_btn_icon, style = fit_model_btn_style))
    } else if (order  %in% c("net") && input$tuning == TRUE) {
      div(style = "flex: 1;margin-right: 20px;",
          h4("Neural Networks - Tuning"),
          selectInput("t_net_predictors", "Quantitative predictors", choices = c(), multiple = T),
          selectInput("t_net_predictors_f", "Qualitative predictors", choices = c(), multiple = T),
          selectInput("t_net_thr", "Threshold", choices = thr, multiple = T),
          conditionalPanel("input.t_net_thr.includes('sensitivity')",
                           numericInput("t_net_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
          #textInput("t_net_formula", "Model formula", value = NULL),
          textInput("t_net_size", label = "Number of units",
                    value = "2, 4, 6, 10, 14", placeholder = "2, 4, 6, 10, 14"),
          textInput("t_net_decay", label = "Weight decay",
                    value = "0.001, 0.05, 0.1, 1, 3, 4, 5, 10", placeholder = "0.001, 0.05, 0.1, 1, 3, 4, 5, 10"),
          selectInput("t_net_metric", "Metric", choices = metric, selected = "TSS"),
          numericInput(inputId = "t_net_n_cores", "Number of cores", min = 1, value = 1),
          actionButton("t_net", "Tune NET", icon = fit_model_btn_icon, style = fit_model_btn_style))
    } else if (order  %in% c("raf") && input$tuning == TRUE) {
      div(style = "flex: 1; margin-right: 20px;",
          h4("Random Forests - Tuning"),
          selectInput("t_raf_predictors", "Quantitative predictors", choices = c(), multiple = T),
          selectInput("t_raf_predictors_f", "Qualitative predictors", choices = c(), multiple = T),
          selectInput("t_raf_thr", "Threshold", choices = thr, multiple = T),
          conditionalPanel("input.t_raf_thr.includes('sensitivity')",
                           numericInput("t_raf_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
          textInput("t_raf_formula", "Model formula"),
          # Grid Construction
          textInput("t_raf_mtry", "Number of variables sampled",
                    value = "1, 2, 3, 4, 5", placeholder = "1, 2, 3, 4, 5"),
          selectInput("t_raf_metric", "Metric", choices = metric, selected = "TSS"),
          numericInput(inputId = "t_raf_n_cores", "Number of cores", min = 1, value = 1),
          actionButton("t_raf", "Tune RAF", icon = fit_model_btn_icon, style = fit_model_btn_style))
    } else if (order  %in% c("svm") && input$tuning == TRUE) {
      div(style = "flex: 1; margin-right: 20px;",
          h4("Support Vector Machine - Tuning"),
          selectInput("t_svm_predictors", "Quantitative predictors", choices = c(), multiple = T),
          selectInput("t_svm_predictors_f", "Qualitative predictors", choices = c(), multiple = T),
          selectInput("t_svm_thr", "Threshold", choices = thr, multiple = T),
          conditionalPanel("input.t_svm_thr.includes('sensitivity')",
                           numericInput("t_svm_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
          textInput("t_svm_formula", "Model formula"),
          # Grid Construction
          textInput("t_svm_sigma", "Inverse kernel width",
                    value = "0.01, 0.1, 0.2, 0.3, 0.4", placeholder = "0.01, 0.1, 0.2, 0.3, 0.4"),
          textInput("t_svm_C", "Cost of constraints violation",
                       value = "2, 4, 8, 16, 20", placeholder = "2, 4, 8, 16, 20"),
          selectInput("t_svm_metric", "Metric", choices = metric, selected = "TSS"),
          numericInput(inputId = "t_svm_n_cores", "Number of cores", min = 1, value = 1),
          actionButton("t_svm", "Tune SVM", icon = fit_model_btn_icon, style = fit_model_btn_style))
    }
    ## FITTING
    else if (order %in% c("gam") && input$tuning == FALSE) {
      div(style = "flex: 1;  margin-right: 20px;",
              h4("Generalized Additive Models"),
              selectInput("gam_predictors", "Quantitative p
                          redictors", choices = c(), multiple = T),
              selectInput("gam_predictors_f", "Qualitative predictors", choices = c(), multiple = T),
              selectInput("gam_select_pred", "Perform predictor selection", choices = c("No" = FALSE, "Yes" = TRUE),
                          selected = FALSE),
              selectInput("gam_thr", "Threshold", choices = thr, multiple = T),
              conditionalPanel("input.gam_thr.includes('sensitivity')",
                               numericInput("gam_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
              textInput("gam_fit_formula", "Model formula"),
              numericInput("gam_k", "Basis dimension", value = -1),
              actionButton("fit_gam", "Fit GAM", icon = fit_model_btn_icon, style = fit_model_btn_style)
      )

    } else if (order  %in% c("gau") && input$tuning == FALSE) {
      div(style = "flex: 1;margin-right: 20px;",
             h4("Gaussian Process Models"),
             selectInput("gau_predictors", "Quantitative predictors", choices = c(), multiple = T),
             selectInput("gau_predictors_f", "Qualitative predictors", choices = c(), multiple = T),
             selectInput("gau_thr", "Threshold", choices = thr, multiple = T),
             conditionalPanel("input.gau_thr.includes('sensitivity')",
                              numericInput("gau_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
             actionButton("fit_gau", "Fit GPM", icon = fit_model_btn_icon, style = fit_model_btn_style))

    } else if (order  %in% c("gbm") && input$tuning == FALSE) {
      div(style = "flex: 1;margin-right: 20px;",
             h4("Generalized Boosted Models"),
             selectInput("gbm_predictors", "Quantitative predictors", choices = c(), multiple = T),
             selectInput("gbm_predictors_f", "Qualitative predictors", choices = c(), multiple = T),
             selectInput("gbm_thr", "Threshold", choices = thr, multiple = T),
             conditionalPanel("input.gbm_thr.includes('sensitivity')",
                              numericInput("gbm_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
             textInput("gbm_fit_formula", "Model formula"),
             numericInput("gbm_n_trees", label = "Number of trees", value = 100, min = 1),
             numericInput("gbm_shrinkage", label = "Learning rate", value = 0.1, min = 0.001, max = 0.1, step = 0.001),
             actionButton("fit_gbm", "Fit GBM", icon = fit_model_btn_icon, style = fit_model_btn_style))

    } else if (order  %in% c("glm") && input$tuning == FALSE) {
      div(style = "flex: 1;margin-right: 20px;",
             h4("Generalized Linear Models"),
             selectInput("glm_predictors", "Quantitative predictors", choices = c(), multiple = T),
             selectInput("glm_predictors_f", "Qualitative predictors", choices = c(), multiple = T),
             selectInput("glm_select_pred", "Perform predictor selection", choices = c("No" = FALSE, "Yes" = TRUE),
                         selected = FALSE),
             selectInput("glm_thr", "Threshold", choices = thr, multiple = T),
             conditionalPanel("input.glm_thr.includes('sensitivity')",
                              numericInput("glm_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
             textInput("glm_fit_formula", "Model formula"),
             numericInput("glm_poly", label = "Polynomials", value = 0, min = 1),
             numericInput("glm_inter_order", label = "Interaction order", value = 0.1, min = 0.001, max = 0.1, step = 0.001),
             actionButton("fit_glm", "Fit GLM", icon = fit_model_btn_icon, style = fit_model_btn_style))
    } else if (order  %in% c("max") && input$tuning == FALSE) {
      div(style = "flex: 1;margin-right: 20px;",
             h4("Maximum Entropy Models"),
             selectInput("max_predictors", "Quantitative predictors", choices = c(), multiple = T),
             selectInput("max_predictors_f", "Qualitative predictors", choices = c(), multiple = T),
             selectInput("max_clamp", "Clamp", choices = c("No" = FALSE, "Yes" = TRUE), selected = FALSE),
             selectInput("max_thr", "Threshold", choices = thr, multiple = T),
             conditionalPanel("input.max_thr.includes('sensitivity')",
                              numericInput("max_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
             #textInput("max_fit_formula", "Model formula", value = NULL),
             selectInput("max_classes", label = "Classes", multiple = T,
                                         c("Default" = "default", "Linear" = "l",
                                           "Quadratic" = "q", "Hinge" = "h", "Product" = "p", "Threshold" = "t"),
                         selected = "default"
                             ),
             selectInput("max_pred_type", label = "Type of response",
                         c("Link" = "link", "Exponential" = "exponential", "Cloglog" = "cloglog", "Logistic" = "logistic"),
                         selected = "cloglog"),
             numericInput("max_regmult", label = "Regularization", value = 1, min = 1),
             actionButton("fit_max", "Fit MAX", icon = fit_model_btn_icon, style = fit_model_btn_style))
    } else if (order  %in% c("net") && input$tuning == FALSE) {
      div(style = "flex: 1;margin-right: 20px;",
             h4("Neural Networks Models"),
             selectInput("net_predictors", "Quantitative predictors", choices = c(), multiple = T),
             selectInput("net_predictors_f", "Qualitative predictors", choices = c(), multiple = T),
             selectInput("net_thr", "Threshold", choices = thr, multiple = T),
             conditionalPanel("input.net_thr.includes('sensitivity')",
                              numericInput("net_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
             #textInput("net_fit_formula", "Model formula"),
             numericInput("net_size", label = "Number of units", value = 2, min = 0),
             numericInput("net_decay", label = "Weight decay", value = 0.1, min = 0, step = 0.01),
             actionButton("fit_net", "Fit NET", icon = fit_model_btn_icon, style = fit_model_btn_style))
    } else if (order  %in% c("raf") && input$tuning == FALSE) {
      div(style = "flex: 1; margin-right: 20px;",
          h4("Random Forests Models"),
          selectInput("raf_predictors", "Quantitative predictors", choices = c(), multiple = T),
          selectInput("raf_predictors_f", "Qualitative predictors", choices = c(), multiple = T),
          selectInput("raf_thr", "Threshold", choices = thr, multiple = T),
          conditionalPanel("input.raf_thr.includes('sensitivity')",
                           numericInput("raf_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
          textInput("raf_fit_formula", "Model formula"),
          actionButton("fit_raf", "Fit RAF", icon = fit_model_btn_icon, style = fit_model_btn_style))
    } else if (order  %in% c("svm") && input$tuning == FALSE) {
      div(style = "flex: 1; margin-right: 20px;",
             h4("Support Vector Machine Models"),
             selectInput("svm_predictors", "Quantitative predictors", choices = c(), multiple = T),
             selectInput("svm_predictors_f", "Qualitative predictors", choices = c(), multiple = T),
             selectInput("svm_thr", "Threshold", choices = thr, multiple = T),
             conditionalPanel("input.svm_thr.includes('sensitivity')",
                              numericInput("svm_sens", label = "Sensitivity value",
                                           value = 0.9, min = 0, max = 1, step = 0.01)),
             textInput("svm_fit_formula", "Model formula"),
             #selectInput("svm_sigma", "Inverse kernel width", choices = c("automatic"), selected = "automatic"),
             numericInput("svm_C", "Cost of constraints violation", value = 1, min = 1),
             actionButton("fit_svm", "Fit SVM", icon = fit_model_btn_icon, style = fit_model_btn_style))
    }else if (order  %in% c("bioc") && input$tuning == FALSE) {
      div(style = "flex: 1; margin-right: 20px;",
          h4("Bioclim Models"),
          selectInput("bioc_response", "Response", choices = c(), multiple = F),
          selectInput("bioc_predictors", "Quantitative predictors", choices = c(), multiple = T),
          selectInput("bioc_thr", "Threshold", choices = thr, multiple = T),
          conditionalPanel("input.bioc_thr.includes('sensitivity')",
                           numericInput("bioc_sens", label = "Sensitivity value",
                                        value = 0.9, min = 0, max = 1, step = 0.01)),
          actionButton("fit_bioc", "Fit BIOC", icon = fit_model_btn_icon, style = fit_model_btn_style))
    }

  })

    do.call(div, c(columns, style = "display: flex;"))

})
