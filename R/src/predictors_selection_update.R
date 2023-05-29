## GAM
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "gam_predictors", choices = colnames(ready_df_mod()))
})
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "gam_predictors_f", choices = colnames(ready_df_mod())[ !colnames(ready_df_mod()) %in% input$gam_predictors])
})
## update for GAM fit_formula placholder
observe({
  req(ready_df_mod())
  if (!is.null(input$gam_predictors_f)) {
    formula_str <- paste("pr_ab", "~", 
                         paste("s(", input$gam_predictors, ")", collapse=" + ", sep = ""), 
                         "+", paste(input$gam_predictors_f, collapse = " + ", sep = ""))
  }else{
    formula_str <- paste("pr_ab", "~", 
                         paste("s(", input$gam_predictors, ")", collapse=" + ", sep = ""))
  }
  updateTextInput(inputId = "gam_fit_formula", value = formula_str)
})
# observe({
#   req(ready_df_mod())
#   updateSelectInput(inputId = "gam_partition", choices = colnames(ready_df_mod())[ !colnames(ready_df_mod()) %in% input$gam_predictors])
# })

## GAU
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "gau_predictors", choices = colnames(ready_df_mod()))
})
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "gau_predictors_f", choices = colnames(ready_df_mod())[ !colnames(ready_df_mod()) %in% input$gau_predictors])
})

## GBM
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "gbm_predictors", choices = colnames(ready_df_mod()))
})
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "gbm_predictors_f", choices = colnames(ready_df_mod())[ !colnames(ready_df_mod()) %in% input$gbm_predictors])
})
## update for GBM fit_formula placholder
observe({
  req(ready_df_mod())
  if (!is.null(input$gbm_predictors_f)) {
    formula_str <- paste("pr_ab", "~", 
                         paste(input$gbm_predictors, collapse = " + ", sep = ""), 
                         "+", paste(input$gbm_predictors_f, collapse = " + ", sep = ""))
  }else{
    formula_str <- paste("pr_ab", "~", 
                         paste(input$gbm_predictors, collapse = " + ", sep = ""))
  }
  updateTextInput(inputId = "gbm_fit_formula", value = formula_str)
})

## GLM
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "glm_predictors", choices = colnames(ready_df_mod()))
})
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "glm_predictors_f", choices = colnames(ready_df_mod())[ !colnames(ready_df_mod()) %in% input$glm_predictors])
})
## update for GBM fit_formula placholder
observe({
  req(ready_df_mod())
  if (!is.null(input$glm_predictors_f)) {
    formula_str <- paste("pr_ab", "~", 
                         paste(input$glm_predictors, collapse = " + ", sep = ""), 
                         "+", paste(input$glm_predictors_f, collapse = " + ", sep = ""))
  }else{
    formula_str <- paste("pr_ab", "~", 
                         paste(input$glm_predictors, collapse = " + ", sep = ""))
  }
  updateTextInput(inputId = "glm_fit_formula", value = formula_str)
})


## MAX
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "max_predictors", choices = colnames(ready_df_mod()))
})
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "max_predictors_f", choices = colnames(ready_df_mod())[ !colnames(ready_df_mod()) %in% input$max_predictors])
})
observe({
  if ("default" %in% input$max_classes) {
    updateSelectInput(inputId = "max_classes", 
                      choices = c("Default" = "default"),
                      selected = "default"
    )
  } else {
    updateSelectInput(inputId = "max_classes", 
                      choices = c("Default" = "default", "Linear" = "l", "Quadratic" = "q", "Hinge" = "h", "Product" = "p", "Threshold" = "t"),
                      selected = input$max_classes
    )
  }
})
## NET
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "net_predictors", choices = colnames(ready_df_mod()))
})
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "net_predictors_f", choices = colnames(ready_df_mod())[ !colnames(ready_df_mod()) %in% input$net_predictors])
})
observe({
  req(ready_df_mod())
  if (!is.null(input$net_predictors_f)) {
    formula_str <- paste("pr_ab", "~", 
                         paste(input$net_predictors, collapse = " + ", sep = ""), 
                         "+", paste(input$net_predictors_f, collapse = " + ", sep = ""))
  }else{
    formula_str <- paste("pr_ab", "~", 
                         paste(input$net_predictors, collapse = " + ", sep = ""))
  }
  updateTextInput(inputId = "net_fit_formula", value = formula_str)
})

## RAF
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "raf_predictors", choices = colnames(ready_df_mod()))
})
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "raf_predictors_f", choices = colnames(ready_df_mod())[ !colnames(ready_df_mod()) %in% input$raf_predictors])
})
observe({
  req(ready_df_mod())
  if (!is.null(input$raf_predictors_f)) {
    formula_str <- paste("pr_ab", "~",
                         paste(input$raf_predictors, collapse = " + ", sep = ""),
                         "+", paste(input$raf_predictors_f, collapse = " + ", sep = ""))
  }else{
    formula_str <- paste("pr_ab", "~",
                         paste(input$raf_predictors, collapse = " + ", sep = ""))
  }
  updateTextInput(inputId = "raf_fit_formula", value = formula_str)
})

## SVM
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "svm_predictors", choices = colnames(ready_df_mod()))
})
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "svm_predictors_f", choices = colnames(ready_df_mod())[ !colnames(ready_df_mod()) %in% input$svm_predictors])
})
observe({
  req(ready_df_mod())
  if (!is.null(input$svm_predictors_f)) {
    formula_str <- paste("pr_ab", "~",
                         paste(input$svm_predictors, collapse = " + ", sep = ""),
                         "+", paste(input$svm_predictors_f, collapse = " + ", sep = ""))
  }else{
    formula_str <- paste("pr_ab", "~",
                         paste(input$svm_predictors, collapse = " + ", sep = ""))
  }
  updateTextInput(inputId = "svm_fit_formula", value = formula_str)
})

################################### ESM ########################################
## GAM -----
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "gam_esm_predictors", choices = colnames(ready_df_mod()))
})


## GAU -------
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "gau_esm_predictors", choices = colnames(ready_df_mod()))
})

## GBM ------
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "gbm_esm_predictors", choices = colnames(ready_df_mod()))
})

## GLM ---------
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "glm_esm_predictors", choices = colnames(ready_df_mod()))
})

## MAX -----------
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "max_esm_predictors", choices = colnames(ready_df_mod()))
})
## NET ---------
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "net_esm_predictors", choices = colnames(ready_df_mod()))
})

## SVM
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "svm_esm_predictors", choices = colnames(ready_df_mod()))
})