## Define predictors from data clomuns name

################################## Standard Models ###########################
predictors <- reactive({
    req(ready_df_mod())
    if(any(startsWith(colnames(ready_df_mod()), ".part"))) {
      colnames(ready_df_mod())[!startsWith(colnames(ready_df_mod()), ".part")]
    }else{colnames(ready_df_mod())}
})
## GAM
observe({
  updateSelectInput(inputId = "gam_predictors", choices = predictors()) })
observe({
  updateSelectInput(inputId = "gam_predictors_f", choices = predictors()[ !predictors() %in% input$gam_predictors]) })
## update for GAM fit_formula placholder
observe({
  if (!is.null(input$gam_predictors_f)) {
    formula_str <- paste("pr_ab", "~",
                         paste("s(", input$gam_predictors, ")", collapse=" + ", sep = ""),
                         "+", paste(input$gam_predictors_f, collapse = " + ", sep = ""))
  }else{
    formula_str <- paste("pr_ab", "~",
                         paste("s(", input$gam_predictors, ")", collapse=" + ", sep = "")) }
  updateTextInput(inputId = "gam_fit_formula", value = formula_str)
})

## GAU
observe({
  updateSelectInput(inputId = "gau_predictors", choices = predictors()) })
observe({
  updateSelectInput(inputId = "gau_predictors_f", choices = predictors()[ !predictors() %in% input$gau_predictors]) })

## GBM
observe({
  updateSelectInput(inputId = "gbm_predictors", choices = predictors()) })
observe({
  updateSelectInput(inputId = "gbm_predictors_f", choices = predictors()[ !predictors() %in% input$gbm_predictors]) })
## update for GBM fit_formula placholder
observe({
  if (!is.null(input$gbm_predictors_f)) {
    formula_str <- paste("pr_ab", "~",
                         paste(input$gbm_predictors, collapse = " + ", sep = ""),
                         "+", paste(input$gbm_predictors_f, collapse = " + ", sep = ""))
  }else{
    formula_str <- paste("pr_ab", "~",
                         paste(input$gbm_predictors, collapse = " + ", sep = ""))
  }
  updateTextInput(inputId = "gbm_fit_formula", value = formula_str) })

## GLM
observe({
  updateSelectInput(inputId = "glm_predictors", choices = predictors()) })
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "glm_predictors_f", choices = predictors()[ !predictors() %in% input$glm_predictors]) })
## update for GLM fit_formula placholder
observe({
  if (!is.null(input$glm_predictors_f)) {
    formula_str <- paste("pr_ab", "~",
                         paste(input$glm_predictors, collapse = " + ", sep = ""),
                         "+", paste(input$glm_predictors_f, collapse = " + ", sep = ""))
  }else{
    formula_str <- paste("pr_ab", "~",
                         paste(input$glm_predictors, collapse = " + ", sep = ""))
  }
  updateTextInput(inputId = "glm_fit_formula", value = formula_str) })


## MAX
observe({
  updateSelectInput(inputId = "max_predictors", choices = predictors()) })
observe({
  updateSelectInput(inputId = "max_predictors_f", choices = predictors()[ !predictors() %in% input$max_predictors]) })
## update for MAX fit_formula placholder

## NET
observe({
  updateSelectInput(inputId = "net_predictors", choices = predictors()) })
observe({
  updateSelectInput(inputId = "net_predictors_f", choices = predictors()[ !predictors() %in% input$net_predictors]) })
observe({
  if (!is.null(input$max_fit_formula)) {
    formula_str <- stats::formula(as.formula(input$max_fit_formula))
  }else{
    formula_str <- NULL
  }
  updateTextInput(inputId = "max_fit_formula", value = formula_str) })

## RAF
observe({
  updateSelectInput(inputId = "raf_predictors", choices = predictors())
})
observe({
  updateSelectInput(inputId = "raf_predictors_f", choices = predictors()[ !predictors() %in% input$raf_predictors])
})
observe({
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
  updateSelectInput(inputId = "svm_predictors", choices = predictors())
})
observe({
  updateSelectInput(inputId = "svm_predictors_f", choices = predictors()[ !predictors() %in% input$svm_predictors])
})
observe({
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

## BIOC
observe({
  updateSelectInput(inputId = "bioc_response", choices = predictors())
})
observe({
  updateSelectInput(inputId = "bioc_predictors", choices = predictors()[ !predictors() %in% input$bioc_response])
})

################################### ESM ########################################
## GAM -----
observe({
  updateSelectInput(inputId = "gam_esm_predictors", choices = predictors())
})


## GAU -------
observe({
  updateSelectInput(inputId = "gau_esm_predictors", choices = predictors())
})

## GBM ------
observe({
  updateSelectInput(inputId = "gbm_esm_predictors", choices = predictors())
})

## GLM ---------
observe({
  updateSelectInput(inputId = "glm_esm_predictors", choices = predictors())
})

## MAX -----------
observe({
  updateSelectInput(inputId = "max_esm_predictors", choices = predictors())
})
## NET ---------
observe({
  updateSelectInput(inputId = "net_esm_predictors", choices = predictors())
})

## SVM
observe({
  updateSelectInput(inputId = "svm_esm_predictors", choices = predictors())
})

############ ------------ TUNING ----------------###################
## GBM
observe({
  updateSelectInput(inputId = "t_gbm_predictors", choices = predictors()) })
observe({
  updateSelectInput(inputId = "t_gbm_predictors_f", choices = predictors()[ !predictors() %in% input$t_gbm_predictors]) })
## update for GBM fit_formula placholder
observe({
  if (!is.null(input$t_gbm_predictors_f)) {
    formula_str <- paste("pr_ab", "~",
                         paste(input$t_gbm_predictors, collapse = " + ", sep = ""),
                         "+", paste(input$t_gbm_predictors_f, collapse = " + ", sep = ""))
  }else{
    formula_str <- paste("pr_ab", "~",
                         paste(input$t_gbm_predictors, collapse = " + ", sep = ""))
  }
  updateTextInput(inputId = "t_gbm_fit_formula", value = formula_str) })

## MAX
observe({
  updateSelectInput(inputId = "t_max_predictors", choices = predictors()) })
observe({
  updateSelectInput(inputId = "t_max_predictors_f", choices = predictors()[ !predictors() %in% input$max_predictors]) })


## NET
observe({
  updateSelectInput(inputId = "t_net_predictors", choices = predictors()) })
observe({
  updateSelectInput(inputId = "t_net_predictors_f", choices = predictors()[ !predictors() %in% input$net_predictors]) })

## RAF
observe({
  updateSelectInput(inputId = "t_raf_predictors", choices = predictors())
})
observe({
  updateSelectInput(inputId = "t_raf_predictors_f", choices = predictors()[ !predictors() %in% input$t_raf_predictors])
})
observe({
  if (!is.null(input$t_raf_predictors_f)) {
    formula_str <- paste("pr_ab", "~",
                         paste(input$t_raf_predictors, collapse = " + ", sep = ""),
                         "+", paste(input$t_raf_predictors_f, collapse = " + ", sep = ""))
  }else{
    formula_str <- paste("pr_ab", "~",
                         paste(input$t_raf_predictors, collapse = " + ", sep = ""))
  }
  updateTextInput(inputId = "t_raf_formula", value = formula_str)
})

## SVM
observe({
  updateSelectInput(inputId = "t_svm_predictors", choices = predictors())
})
observe({
  updateSelectInput(inputId = "t_svm_predictors_f", choices = predictors()[ !predictors() %in% input$svm_predictors])
})
observe({
  if (!is.null(input$t_svm_predictors_f)) {
    formula_str <- paste("pr_ab", "~",
                         paste(input$t_svm_predictors, collapse = " + ", sep = ""),
                         "+", paste(input$t_svm_predictors_f, collapse = " + ", sep = ""))
  }else{
    formula_str <- paste("pr_ab", "~",
                         paste(input$t_svm_predictors, collapse = " + ", sep = ""))
  }
  updateTextInput(inputId = "t_svm_formula", value = formula_str)
})

