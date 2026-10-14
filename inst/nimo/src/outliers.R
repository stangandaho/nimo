# Environmental outliers

outliers_modal <- function() {
  modalDialog(
    title = h3("Environmental outliers", style = "text-align:left"), size = "l",
    footer = tagList(
      modalButton("Ok"),
      actionButton("detect_outliers", "Detect", icon = icon("search"), style = bttn_primary_style)
    ),
    fluidRow(
      column(8,
             tabsetPanel(
               tabPanel("Map", shinycssloaders::withSpinner(plotOutput("outliers_plot", height = "420px"),
                                                           color = loader_color, type = loader_type)),
               tabPanel("Methods", DT::DTOutput("outliers_summary")),
               tabPanel("Flagged records", DT::DTOutput("outliers_table"))
             )),
      column(4,
             p("Presences with unusual environmental conditions are flagged by five methods:",
               "boxplot, reverse jackknife, support vector machine, random forest and",
               "random-forest outlier score (support vector machine and random forest need absences",
               "or background points). Coordinates must be in the predictors' coordinate system.",
               style = "color:#5f6b6d"),
             numericInput("outliers_min_methods", "Remove presences flagged by at least",
                          value = 3, min = 1, max = 5, step = 1),
             p("of the 5 methods", style = "margin-top:-10px; color:#5f6b6d"),
             actionButton("apply_outliers", "Remove flagged", icon = icon("filter"), style = bttn_second_style),
             actionButton("reset_outliers", "Keep all", icon = icon("undo")),
             br(), br(),
             strong(textOutput("outliers_status"))
      )
    )
  )
}
observeEvent(input$outliers_btn, showModal(outliers_modal()))

## Minimum number of methods flagging a presence to remove it (NULL: keep all)
outlier_rule <- reactiveVal(NULL)

outliers_res <- eventReactive(input$detect_outliers, {
  req(wrangle_data(), env_base())
  tryCatch({
    code <- paste0(
      "occ_id <- dplyr::mutate(occ, .id = seq_len(nrow(occ)))\n",
      "set.seed(seed)\n",
      "occ_out <- flexsdm::env_outliers(\n",
      "  data = occ_id,\n",
      "  x = ", nm_lit(input$long_var), ",\n",
      "  y = ", nm_lit(input$lat_var), ",\n",
      "  pr_ab = \"pr_ab\",\n",
      "  id = \".id\",\n",
      "  env_layer = env\n)")
    outlier_rule(NULL)
    forget_step("outliers_remove")
    reset_filter_use()
    set.seed(global_seed())
    env <- run_code("outliers", "outliers", "Detection (presences flagged by 5 methods)", code,
                    list(occ = wrangle_data()[[1]], env = env_base(), seed = global_seed()))
    env$occ_out
  }, error = error)
})

observeEvent(input$apply_outliers, {
  req(is.data.frame(outliers_res()))
  k <- input$outliers_min_methods
  outlier_rule(k)
  reset_filter_use()
  record_step("outliers_remove", "outliers", "Removal", c(
    paste0("# Remove presences flagged as outliers by at least ", k, " of the 5 methods"),
    "occ <- occ_out %>%",
    paste0("  dplyr::filter(is.na(.out_sum) | .out_sum < ", k, ") %>%"),
    "  dplyr::select(-.id, -dplyr::starts_with(\".out_\"))"
  ))
})
observeEvent(input$reset_outliers, {
  outlier_rule(NULL)
  forget_step("outliers_remove")
  reset_filter_use()
})

## Records after outlier removal
occ_clean <- reactive({
  k <- outlier_rule()
  if (is.null(k)) return(wrangle_data()[[1]])
  outliers_res() %>%
    dplyr::filter(is.na(.out_sum) | .out_sum < k) %>%
    dplyr::select(-.id, -dplyr::starts_with(".out_"))
})

output$outliers_status <- renderText({
  n <- tryCatch(nrow(wrangle_data()[[1]]), error = function(e) NULL)
  if (is.null(n)) return("No records loaded yet.")
  k <- outlier_rule()
  if (is.null(k)) return(paste("All", n, "records are used."))
  paste0(n - nrow(occ_clean()), " presence(s) removed (flagged by at least ", k,
         " methods); ", nrow(occ_clean()), " records are used.")
})

output$outliers_plot <- renderPlot({
  out <- outliers_res()
  req(is.data.frame(out))
  out$methods <- factor(ifelse(is.na(out$.out_sum), "no data", out$.out_sum),
                        levels = c(0:5, "no data"))
  ggplot(out, aes(x = .data[[input$long_var]], y = .data[[input$lat_var]])) +
    geom_point(data = out[out$pr_ab == 0, ], colour = "grey80", size = 1) +
    geom_point(data = out[out$pr_ab == 1, ], aes(colour = methods), size = 2) +
    scale_colour_manual(values = c("0" = "#042d0f", "1" = "#7a9a01", "2" = "#f2c14e",
                                   "3" = "#f26802", "4" = "#c52323", "5" = "#6b0f0f",
                                   "no data" = "#9aa3a6"),
                        drop = FALSE, name = "Flagged by\n(methods)") +
    labs(x = "Longitude", y = "Latitude",
         caption = "Light grey: absences / background points. No data: outside the predictors") +
    theme_minimal()
})

output$outliers_summary <- render_dt({
  out <- outliers_res()
  req(is.data.frame(out))
  pres <- out[out$pr_ab == 1, ]
  data.frame(
    Method = c("Boxplot", "Reverse jackknife", "Support vector machine", "Random forest",
               "Random-forest outlier score"),
    `Presences flagged` = vapply(c(".out_bxpt", ".out_jack", ".out_svm", ".out_rf", ".out_rfout"),
                                 function(v) sum(pres[[v]] == 1, na.rm = TRUE), numeric(1)),
    check.names = FALSE
  )
})

output$outliers_table <- st_render_dt({
  out <- outliers_res()
  req(is.data.frame(out))
  out[out$pr_ab == 1 & !is.na(out$.out_sum) & out$.out_sum > 0, ] %>%
    dplyr::arrange(dplyr::desc(.out_sum))
})
