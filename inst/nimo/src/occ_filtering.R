# Occurrence filtering

occ_filt_modal <- function(){
  modalDialog(title = h3("Occurrence data filtering", style = "text-align:left"), size = "l",
              footer = tagList(
                modalButton("Ok"),
                actionButton("valid_occ_filt", "Filter", icon = icon("check"), style = bttn_primary_style)
              ),
              fluidRow(column(8,
                              tabsetPanel(
                                id = "occ_filt_tabs",
                                tabPanel("Distribution",
                                         selectInput("enlayer_to_plot", label = NULL, choices = c()),
                                         plotOutput("occ_filt_plot")),
                                tabPanel("Selection", DT::DTOutput("occ_filt_selection"),
                                         p("Several filtering values: the dataset marked * has a spatial",
                                           "autocorrelation below the mean and the most records (flexsdm::occfilt_select).",
                                           style = "color:#5f6b6d; margin-top:10px")),
                                tabPanel("Filtered data",
                                         DT::dataTableOutput("occ_filtered_data", height = "500px"))
                              )),
                       column(4,
                              selectInput("occ_filt_type", "Filtering type", choices = c("Geographical", "Environmental")),
                              conditionalPanel("input.occ_filt_type == 'Environmental'",
                                               textInput("occ_filt_nbins", "Number of bins", value = "4")),
                              conditionalPanel("input.occ_filt_type == 'Geographical'",
                                               selectInput("occ_filt_method", "Filtering method",
                                                           choices = occ_filt_method),
                                               conditionalPanel("input.occ_filt_method == 'cellsize'",
                                                                textInput("occ_filt_cellsize", "Factor", value = "2")),
                                               conditionalPanel("input.occ_filt_method == 'defined'",
                                                                textInput("occ_filt_defined", "Distance (km)", value = "1"))),
                              conditionalPanel("input.occ_filt_type == 'Environmental' || input.occ_filt_method != 'moran'",
                                               p("Several values separated by commas (e.g. 2, 4, 8) are all tried and the best",
                                                 "filtered dataset is selected.", style = "color:#5f6b6d")),
                              hr(),
                              actionButton("use_occ_filt", "Use in analysis", icon = icon("check-double"),
                                           style = bttn_second_style),
                              actionButton("unuse_occ_filt", "Don't use", icon = icon("undo")),
                              br(), br(),
                              strong(textOutput("occ_filt_status")),
                              br(),
                              shinySaveButton(id = "save_occ_filt", label = "Save",  title = "Save occurrence data filtered",
                                              filename = "", filetype = list(CSV = "csv", `Plain text` = "txt"),
                                              icon = icon("save"), style = bttn_primary_style)
                       ))
  )
}

observeEvent(input$occ_filt, {
  showModal(occ_filt_modal())
  updateSelectInput(session, "enlayer_to_plot", choices = names(env_base()))
})
### Download setting
shinyFiles::shinyFileSave(input, "save_occ_filt", roots = root, session = session)
save_occ_filt_path <- reactive({
  req(input$save_occ_filt)
  shinyFiles::parseSavePath(roots = root, selection = input$save_occ_filt)
})
observe({
  req(save_occ_filt_path())
  if (nrow(save_occ_filt_path()) > 0 ) {
    write.csv(x = occ_filt_out()$occ_filt, file = as.character(save_occ_filt_path()$datapath))
  }
})

## Filtering values ("2, 4, 8")
filt_values <- function(txt) {
  v <- trimws(strsplit(as.character(txt), ",")[[1]])
  v <- v[nzchar(v)]
  if (length(v) == 0 || anyNA(suppressWarnings(as.numeric(v)))) {
    stop("Filtering values must be numbers separated by commas")
  }
  v
}

occ_filt_out <- eventReactive(input$valid_occ_filt, {
  req(occ_clean(), env_base())
  tryCatch({
    lon <- nm_lit(input$long_var); lat <- nm_lit(input$lat_var)
    if (input$occ_filt_type == "Environmental") {
      values <- filt_values(input$occ_filt_nbins)
      call <- paste0(
        "occ_filt <- flexsdm::occfilt_env(\n",
        "  data = occ_id[occ_id$pr_ab == 1, ],\n  x = ", lon, ",\n  y = ", lat, ",\n",
        "  id = \".id\",\n  env_layer = env,\n  nbins = ", nm_lit(as.numeric(values)), "\n)")
      title <- paste("Environmental filtering, bins:", paste(values, collapse = ", "))
    } else {
      values <- switch(input$occ_filt_method,
                       "moran" = character(0),
                       "cellsize" = filt_values(input$occ_filt_cellsize),
                       "defined" = filt_values(input$occ_filt_defined))
      method <- switch(input$occ_filt_method,
                       "moran" = "moran",
                       "cellsize" = c("cellsize", factor = values),
                       "defined" = c("defined", d = values))
      call <- paste0(
        "occ_filt <- flexsdm::occfilt_geo(\n",
        "  data = occ_id[occ_id$pr_ab == 1, ],\n  x = ", lon, ",\n  y = ", lat, ",\n",
        "  env_layer = env,\n  method = ", nm_lit(method), ",\n  prj = terra::crs(env)\n)")
      title <- paste0("Geographical filtering (", input$occ_filt_method, ")",
                      if (length(values) > 0) paste0(": ", paste(values, collapse = ", ")) else "")
    }
    code <- c("occ_id <- dplyr::mutate(occ, .id = seq_len(nrow(occ)))",
              "set.seed(seed)",
              call)
    if (length(values) > 1) {
      code <- c(code,
                "# Dataset with low spatial autocorrelation and most records",
                paste0("occ_filt_sel <- flexsdm::occfilt_select(\n  occ_list = occ_filt,\n  x = ", lon,
                       ",\n  y = ", lat, ",\n  env_layer = env,\n  filter_prop = TRUE\n)"),
                "occ_filt_sel$filter_prop",
                "occ_filt <- occ_filt_sel$occ")
    }
    reset_filter_use()
    set.seed(global_seed())
    env <- run_code("occ_filt", "filtering", title, paste(code, collapse = "\n"),
                    list(occ = occ_clean(), env = env_base(), seed = global_seed()))
    list(occ_id = env$occ_id, occ_filt = env$occ_filt,
         filter_prop = if (length(values) > 1) env$occ_filt_sel$filter_prop else NULL)
  }, error = error)
})

observeEvent(input$valid_occ_filt, {
  req(is.list(occ_filt_out()))
  updateTabsetPanel(session, "occ_filt_tabs",
                    selected = if (is.null(occ_filt_out()$filter_prop)) "Distribution" else "Selection")
})

output$occ_filt_plot <- renderPlot({
  req(is.list(occ_filt_out()), input$enlayer_to_plot)
  terra::plot(env_base()[[input$enlayer_to_plot]], main = input$enlayer_to_plot)
  points(occ_filt_out()$occ_filt[c(input$long_var, input$lat_var)], pch = 19, cex = 0.7)
})
output$occ_filtered_data <- DT::renderDataTable({ occ_filt_out()$occ_filt })
output$occ_filt_selection <- render_dt({
  req(is.list(occ_filt_out()), !is.null(occ_filt_out()$filter_prop))
  fp <- as.data.frame(occ_filt_out()$filter_prop)
  num <- vapply(fp, is.numeric, logical(1))
  fp[num] <- lapply(fp[num], round, digits = 4)
  fp
})

## Use of the filtered records ----
filtered_occ <- reactiveVal(NULL)
reset_filter_use <- function() {
  filtered_occ(NULL)
  forget_step("filter_use")
}

observeEvent(input$use_occ_filt, {
  req(is.list(occ_filt_out()))
  code <- c("# Filtered presences, all absences",
            "occ <- occ_id[occ_id$pr_ab == 0 | occ_id$.id %in% occ_filt$.id, ] %>% dplyr::select(-.id)")
  env <- run_code("filter_use", "filtering", "Use in analysis", paste(code, collapse = "\n"),
                  list(occ_id = occ_filt_out()$occ_id, occ_filt = occ_filt_out()$occ_filt))
  filtered_occ(env$occ)
})
observeEvent(input$unuse_occ_filt, reset_filter_use())

## Records used by every later step
occ_used <- reactive({
  f <- filtered_occ()
  if (is.null(f)) occ_clean() else f
})

output$occ_filt_status <- renderText({
  n <- tryCatch(nrow(occ_clean()), error = function(e) NULL)
  if (is.null(n)) return("No records loaded yet.")
  if (is.null(filtered_occ())) return(paste("Filtering not used:", n, "records."))
  paste0("Filtered records used: ", nrow(filtered_occ()), " of ", n, " records.")
})
