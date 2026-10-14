# Reproducible script recorder

nm_cv <- nimo:::nm_cv
nm_cv_values <- nimo:::nm_cv_values
nm_lit <- nimo:::nm_lit
nm_code_call <- nimo:::nm_code_call
nm_script_build <- nimo:::nm_script_build

script_rec <- reactiveValues(steps = list(), seq = 0)

## Store (or replace) the code of a step
record_step <- function(key, section, title, code) {
  isolate({
    steps <- script_rec$steps
    seq <- script_rec$seq + 1
    steps[[key]] <- list(section = section, title = title,
                         code = paste(code, collapse = "\n"), seq = seq)
    script_rec$steps <- steps
    script_rec$seq <- seq
  })
  invisible(NULL)
}

## Drop steps (e.g. a model removed from the session)
forget_step <- function(keys) {
  isolate({
    script_rec$steps <- script_rec$steps[setdiff(names(script_rec$steps), keys)]
  })
}

## Call a function and record the call (nm_cv() arguments: value in the app, code in the script)
run_recorded <- function(key, section, title, assign, fun_name, fun, args, pre = NULL, post = NULL) {
  out <- do.call(fun, nm_cv_values(args))
  record_step(key, section, title, c(pre, nm_code_call(fun_name, args, assign), post))
  out
}

## Run R code in the app and record it
run_code <- function(key, section, title, code, objects = list()) {
  env <- new.env(parent = parent.frame())
  for (nm in names(objects)) assign(nm, objects[[nm]], envir = env)
  eval(parse(text = code), envir = env)
  record_step(key, section, title, code)
  env
}

## Fit and record a model
fit_recorded <- function(display_name, fun, args) {
  var <- model_var(display_name)
  pkg <- if (fun == "nm_fit_bioclim") "nimo" else "flexsdm"
  set.seed(global_seed())
  run_recorded(paste0("model:", var), "modeling", display_name, var,
               paste0(pkg, "::", fun), getExportedValue(pkg, fun), args, pre = "set.seed(seed)")
}

## Tuning grid
grid_cv <- function(...) {
  values <- list(...)
  nm_cv(expand.grid(values), sub("^list\\(", "expand.grid(", nm_lit(values)))
}

## Threshold argument (with optional sensitivity value)
thr_arg <- function(thr, sens) {
  if (any(thr %in% "sensitivity")) c(thr, "sens" = as.character(sens)) else thr
}

## Script variable name of a fitted model
model_var <- function(display_name) {
  key <- tolower(display_name)
  alg <- c("additive" = "gam", "gaussian" = "gau", "boosted" = "gbm", "linear" = "glm",
           "entropy" = "max", "neural" = "net", "random" = "raf", "support" = "svm",
           "bioclim" = "bioc", "domain" = "dom")
  a <- alg[vapply(names(alg), grepl, logical(1), x = key)][1]
  if (is.na(a)) a <- make.names(key)
  if (grepl("esm", key)) return(paste0("esm_", a))
  if (grepl("tuned", key)) return(paste0("m_", a, "_tuned"))
  paste0("m_", a)
}

## Full script
script_text <- reactive({
  nm_script_build(script_rec$steps, seed = global_seed(),
                  session_info = isTRUE(input$script_session_info))
})

## Outputs ----
output$script_preview <- renderText({ script_text() })

output$script_steps <- renderUI({
  steps <- script_rec$steps
  if (length(steps) == 0) {
    return(p("No step recorded yet. Run any step in the app and its code appears here.",
             style = "color:#5f6b6d"))
  }
  sections <- nimo:::nm_script_sections()
  rank <- match(vapply(steps, `[[`, "", "section"), names(sections))
  steps <- steps[order(rank, vapply(steps, `[[`, 0, "seq"))]
  tags$ol(lapply(steps, function(s) {
    tags$li(strong(sections[[s$section]]), if (nzchar(s$title)) paste0(" - ", s$title))
  }), style = "padding-left:18px")
})

output$download_script <- downloadHandler(
  filename = function() paste0("nimo_workflow_", format(Sys.time(), "%Y%m%d_%H%M"), ".R"),
  content = function(file) writeLines(script_text(), file, useBytes = TRUE),
  contentType = "text/plain"
)

observe({
  copy_button_update(session, id = "copy_script_btn", label = "Copy",
                     icon = icon("copy"), text = script_text())
})

observeEvent(input$reset_script, {
  showModal(modalDialog(
    title = "Clear recorded steps?",
    "The script will start again from the next step you run. Results in the app are not affected.",
    footer = tagList(modalButton("Cancel"),
                     actionButton("reset_script_confirm", "Clear", style = bttn_primary_style))
  ))
})
observeEvent(input$reset_script_confirm, {
  script_rec$steps <- list()
  removeModal()
})
