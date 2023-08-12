thr <- c("No omission" = "lpt", "Sensitivity = specificity" = "equal_sens_spec",
         "TSS" = "max_sens_spec", "Jaccard" = "max_jaccard",
         "Sorensen" = "max_sorensen", "FPB" = "max_fpb", "Sensitivity" = "sensitivity")
fit_model_btn_style <- "border-radius: 5px 5px;"
fit_model_btn_icon = icon("buromobelexperte", class = "fit_model_btn_icon")

output$dynamic_esm_model_fitting <- renderUI({
  selected_order <- input$fit_model_algorithm

  columns <- lapply(selected_order, function(order) {
    if (order %in% c("gam") && input$esm == TRUE) {
      div(style = "flex: 1;  margin-right: 20px;",
          h4("Generalized Additive Models - ESM"),
          selectInput("gam_esm_predictors", "Quantitative p
                          redictors", choices = c(), multiple = TRUE),
          selectInput("gam_esm_predictors_f", "Qualitative predictors", choices = c(), multiple = TRUE),
          selectInput("gam_esm_select_pred", "Perform predictor selection", choices = c("No" = FALSE, "Yes" = TRUE),
                      selected = FALSE),
          selectInput("gam_esm_thr", "Threshold", choices = thr, multiple = TRUE),
          conditionalPanel("input.gam_esm_thr.includes('sensitivity')",
                           numericInput("gam_esm_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
          textInput("gam_esm_fit_formula", "Model formula"),
          numericInput("gam_esm_k", "Basis dimension", value = -1),
          actionButton("fit_esm_gam", "Fit GAM - ESM", icon = fit_model_btn_icon, style = fit_model_btn_style)
      )

    } else if (order  %in% c("gau") && input$esm == TRUE) {
      div(style = "flex: 1;margin-right: 20px;",
          h4("Gaussian Process Models - ESM"),
          selectInput("gau_esm_predictors", "Quantitative predictors", choices = c(), multiple = T),
          selectInput("gau_esm_predictors_f", "Qualitative predictors", choices = c(), multiple = T),
          selectInput("gau_esm_thr", "Threshold", choices = thr, multiple = T),
          conditionalPanel("input.gau_esm_thr.includes('sensitivity')",
                           numericInput("gau_esm_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
          actionButton("fit_esm_gau", "Fit GPM - ESM", icon = fit_model_btn_icon, style = fit_model_btn_style))

    } else if (order  %in% c("gbm") && input$esm == TRUE) {
      div(style = "flex: 1;margin-right: 20px;",
          h4("Generalized Boosted Models - ESM"),
          selectInput("gbm_esm_predictors", "Quantitative predictors", choices = c(), multiple = T),
          selectInput("gbm_esm_predictors_f", "Qualitative predictors", choices = c(), multiple = T),
          selectInput("gbm_esm_thr", "Threshold", choices = thr, multiple = T),
          conditionalPanel("input.gbm_esm_thr.includes('sensitivity')",
                           numericInput("gbm_esm_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
          textInput("gbm_esm_fit_formula", "Model formula"),
          numericInput("gbm_esm_n_trees", label = "Number of trees", value = 100, min = 1),
          numericInput("gbm_esm_shrinkage", label = "Learning rate", value = 0.1, min = 0.001, max = 0.1, step = 0.001),
          actionButton("fit_esm_gbm", "Fit GBM - ESM", icon = fit_model_btn_icon, style = fit_model_btn_style))

    } else if (order  %in% c("glm") && input$esm == TRUE) {
      div(style = "flex: 1;margin-right: 20px;",
          h4("Generalized Linear Models - ESM"),
          selectInput("glm_esm_predictors", "Quantitative predictors", choices = c(), multiple = T),
          selectInput("glm_esm_predictors_f", "Qualitative predictors", choices = c(), multiple = T),
          selectInput("glm_esm_select_pred", "Perform predictor selection", choices = c("No" = FALSE, "Yes" = TRUE),
                      selected = FALSE),
          selectInput("glm_esm_thr", "Threshold", choices = thr, multiple = T),
          conditionalPanel("input.glm_esm_thr.includes('sensitivity')",
                           numericInput("glm_esm_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
          textInput("glm_esm_fit_formula", "Model formula"),
          numericInput("glm_esm_poly", label = "Polynomials", value = 0, min = 1),
          numericInput("glm_esm_inter_order", label = "Interaction order", value = 0.1, min = 0.001, max = 0.1, step = 0.001),
          actionButton("fit_esm_glm", "Fit GLM - ESM", icon = fit_model_btn_icon, style = fit_model_btn_style))
    } else if (order  %in% c("max") && input$esm == TRUE) {
      div(style = "flex: 1;margin-right: 20px;",
          h4("Maximum Entropy Models - ESM"),
          selectInput("max_esm_predictors", "Quantitative predictors", choices = c(), multiple = T),
          selectInput("max_esm_predictors_f", "Qualitative predictors", choices = c(), multiple = T),
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
          numericInput("max_esm_regmult", label = "Regularization", value = 1, min = 1),
          actionButton("fit_esm_max", "Fit MAX - ESM", icon = fit_model_btn_icon, style = fit_model_btn_style))
    } else if (order  %in% c("net") && input$esm == TRUE) {
      div(style = "flex: 1;margin-right: 20px;",
          h4("Neural Networks Models - ESM"),
          selectInput("net_esm_predictors", "Quantitative predictors", choices = c(), multiple = T),
          selectInput("net_esm_predictors_f", "Qualitative predictors", choices = c(), multiple = T),
          selectInput("net_esm_thr", "Threshold", choices = thr, multiple = T),
          conditionalPanel("input.net_esm_thr.includes('sensitivity')",
                           numericInput("net_esm_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
          textInput("net_esm_fit_formula", "Model formula"),
          numericInput("net_esm_size", label = "Number of units", value = 2, min = 0),
          numericInput("net_esm_decay", label = "Weight decay", value = 0.1, min = 0, step = 0.01),
          actionButton("fit_esm_net", "Fit NET - ESM", icon = fit_model_btn_icon, style = fit_model_btn_style))
    } else if (order  %in% c("raf") && input$esm == TRUE) {
      div(style = "flex: 1; margin-right: 20px;",
          h4("Random Forests Models - ESM"),
          selectInput("raf_esm_predictors", "Quantitative predictors", choices = c(), multiple = T),
          selectInput("raf_esm_predictors_f", "Qualitative predictors", choices = c(), multiple = T),
          selectInput("raf_esm_thr", "Threshold", choices = thr, multiple = T),
          conditionalPanel("input.raf_esm_thr.includes('sensitivity')",
                           numericInput("raf_esm_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
          textInput("raf_esm_fit_formula", "Model formula"),
          actionButton("fit_esm_raf", "Fit RAF - ESM", icon = fit_model_btn_icon, style = fit_model_btn_style))
    } else if (order  %in% c("svm") && input$esm == TRUE) {
      div(style = "flex: 1; margin-right: 20px;",
          h4("Support Vector Machine Models - ESM"),
          selectInput("svm_esm_predictors", "Quantitative predictors", choices = c(), multiple = T),
          selectInput("svm_esm_predictors_f", "Qualitative predictors", choices = c(), multiple = T),
          selectInput("svm_esm_thr", "Threshold", choices = thr, multiple = T),
          conditionalPanel("input.svm_esm_thr.includes('sensitivity')",
                           numericInput("svm_esm_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
          textInput("svm_esm_fit_formula", "Model formula"),
          #selectInput("svm_sigma", "Inverse kernel width", choices = c("automatic"), selected = "automatic"),
          numericInput("svm_esm_C", "Cost of constraints violation", value = 1, min = 1),
          actionButton("fit_esm_svm", "Fit SVM - ESM", icon = fit_model_btn_icon, style = fit_model_btn_style))
    }
  })

  do.call(div, c(columns, style = "display: flex;"))

})
