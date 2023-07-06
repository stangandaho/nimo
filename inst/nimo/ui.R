## load necessaries packages
pkg_root <- "./src"#paste0(system.file("", package = "nimo"), "/nimo/src")
nimo_logo <- "nimo/nimo_logo.png"; gbif_white_logo <- "nimo/gbif_white_logo.png"
suppressPackageStartupMessages(
  source("./inst/nimo/src/packages.R")
)
thr <- c("No omission" = "lpt", "Sensitivity = specificity" = "equal_sens_spec",
         "TSS" = "max_sens_spec", "Jaccard" = "max_jaccard",
         "Sorensen" = "max_sorensen", "FPB" = "max_fpb",
         "Sensitivity" = "sensitivity")

bttn_primary_style <-  paste0("background-color:", "#58126b;", "color:#ffffff;")
bttn_second_style <- paste0("background-color:", "#ff9e15;", "color:#ffffff;")
loader_color <- "#1b105a"; loader_type  <- 7

## GBIF Queries fields
source("./inst/nimo/src/query_gbif_occ_data.R")

git_repo <- "https://github.com/stangandaho/nimo"
git_issues <- "https://github.com/stangandaho/nimo/issues"
mytitle <- tags$img(src= nimo_logo, height = '30',width='80')
  # tags$link(tags$a(href = git_repo, target="_blank",
  #                           tags$img(src= "./R/www/nimo_logo.png", height = '30',width='80')),
  #                    strong("NIMO"))

colinearity_method <- c("Pearson correlation" = "pearson", "Variance inflation factor" = "vif",
                        "Principal component analysis" = "pca", "Factorial analysis" = "fa")
partition_types <- c("Random" = "part_random", "Spatial band" = "part_sband",
                      "Spatial block" = "part_sblock", "Environmental and Spatial" = "part_senv")
## Header ----
header <- shinydashboardPlus::dashboardHeader(title = mytitle,
                                              titleWidth = 300,
                                              dropdownMenuOutput("notification_menu"),
                                              dropdownMenu(
                                                type = "messages",
                                                icon = icon("question-circle", "font-awesome"),
                                                badgeStatus = NULL,
                                                headerText = strong("Help"),
                                                boxDropdownItem(
                                                  "Get started",
                                                  href = "#",
                                                  icon = icon("menu-right", lib = "glyphicon")
                                                ),
                                                boxDropdownItem(
                                                  "Issues",
                                                  href = git_issues,
                                                  icon = icon("bug", lib = "font-awesome"),
                                                ),
                                                boxDropdownItem(
                                                  "About",
                                                  href = "#",
                                                  icon = icon("info", lib = "font-awesome"),
                                                )
                                              ),
                                              dropdownMenu(
                                                 type = "messages",
                                                 icon = icon("sack-dollar", lib = "font-awesome"),
                                                 badgeStatus = NULL,
                                                 headerText = strong("Donate"),
                                                 boxDropdownItem(
                                                   "Donation",
                                                   href = "#",
                                                   icon = icon("hand-holding-dollar", lib = "font-awesome")
                                                           )
                                              ),
                                              tags$li(class = "dropdown",
                                                       tags$style(".main-header .logo {height: 53px}")
                                              )
)

## Side Bar ----
sidebar <- shinydashboardPlus::dashboardSidebar(
  useShinyjs(),
  sidebarMenu(
    id="sidebar_menu",
    menuItem("Niche Modeler", tabName = "nimo_home_page", icon = icon("house-chimney")),
    menuItem(
        tags$span(
        tags$img(src = gbif_white_logo, height = '26',width='75')),
        tabName = "gbif_access", icon = NULL),
    #menuItem("GBIF Data", tabName = "gbif_access", icon = NULL),
    menuItem("Pre-Modeling",tabName = "pre_modeling", icon = icon("arrow-left", lib = "glyphicon"),
             br(),
             actionButton("set_directory", "Set Directory", icon = icon("folder")),
             menuSubItem("Calibration", tabName = "calibration", icon = NULL),
             menuSubItem("Predictors", tabName = "predictors", icon = NULL), # to move
             menuSubItem("Partition", tabName = "data_partition", icon = NULL),
             menuSubItem("Data extraction", tabName = "data_extraction", icon = NULL)
             ),
    #menuItemOutput("rest_of_sidebar"),
    ## MODELING
    menuItem("Modeling", tabName = "sd_modeling", icon = icon("play", lib = "glyphicon"),
             menuSubItem("Fitting", tabName = "model_fiting", icon = NULL),
             menuSubItem("Ensembling", tabName = "model_ensembling", icon = NULL)
             ),
    menuItem("Post-Modeling", tabName = "post_modeling", icon = icon("arrow-right", lib = "glyphicon"),
             menuSubItem("Spatial predictions", tabName = "spatial_predict", icon = NULL),
             menuSubItem("Extrapolation ", tabName = "extrapolation", icon = NULL),
             menuSubItem("Overprediction correction", tabName = "overpredict_correct", icon = NULL)
             )
  ),
  textOutput("res"),
  width = 300
)



## Body ####
### Theme ----
### Body Content ----
nimo_body <- shinydashboard::dashboardBody(
 # customTheme,
  tags$head(
    tags$script(src = "www/leaflet_base.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css"),
    tags$style(".scrolling-container {
      display: flex;
      flex-wrap: nowrap;
      overflow-x: scroll;
      width: 100%;
    }"),
    tags$style("#models_model {height:300px;}"),
    tags$style("#bpas_check {background-color:#feb4b4; border-radius:10px 10px;
               border: 2px solid #fd7769; padding: 8px; width:75%;}"),
    tags$style("#part_out {height:300px;}"),
    tags$style(("#cr_remove {height:200px;}")),
    tags$style(("#cr_layer {height:200px;}")),
    tags$style("#dir_setup_success {height: 300px;}"),
    tags$style("#pred_summary {height: 400px;}"),
    tags$style(".nav-tabs {margin-bottom: 25px}"),
    tags$style(".treeview-menu {font-size: 0px}"),
    tags$style(".sidebar-menu {font-size: 18px}"),
    tags$style(".sidebar-toggle {font-size: 25px}"),
    tags$script(
      'var dimension = [0, 0];
              $(document).on("shiny:connected", function(e) {
                  dimension[0] = window.innerWidth;
                  dimension[1] = window.innerHeight;
                  Shiny.onInputChange("dimension", dimension);
              });
              $(window).resize(function(e) {
                  dimension[0] = window.innerWidth;
                  dimension[1] = window.innerHeight;
                  Shiny.onInputChange("dimension", dimension);
              });
              $(document).ready(function(){
                  $("a[data-toggle=tab]").on("show.bs.tab", function(e){
                    Shiny.setInputValue("activeTab", $(this).attr("data-value"));
                   });
            });
      '
    )),
  tabItems(
    #### Homepage menu ----
    tabItem("nimo_home_page",
            fluidPage(
              fluidRow(
                column(12,
                       div(h1("Species distribution modeling (SDM) App",
                              style="text-align:center; font-size:50px;")),
                       hr()
                ),
                column(12,
                       div(shiny::img(src = nimo_logo, height = "35%",width = "40%"), style="text-align: center;")
                ),
                br(),
                column(12,
                       div(p("To start quickly with NIMO, please visite the start page ",
                             em(a("here",
                                  href = "#", target="_blank")),
                             style="text-align:center; font-size:18px;")),
                ),
                hr(),
                column(12,
                       div(h6("nimo is open-source and freely available for use, distributed under the GPL license.",
                              style="text-align:center; font-size:19px;")),
                       div(h6("When they are used in a publication, we ask that authors to cite the following reference:",
                              style="text-align:center; font-size:19px;")),
                       br(),
                       div(h6("GANDAHO, S.M. . (2023).", strong(" nimo: A GUI for flexible"),
                              style="text-align:center; font-size:19px;")),
                       div(h6(strong("species distribution modeling."),
                              em("Journal of ..."),", xx(6), xxx-xxx.",
                              style="text-align:center; font-size:19px;")),
                       br(),
                       div(h6("Failure to properly cite the software is considered a violation of the license.",
                              style="text-align:center; font-size:19px;"))
                )

              )
            )
    ),
    ## Calibration menu --------
    tabItem("calibration",
            fluidPage(
              fluidRow(column(8,
                              tabsetPanel(
                                tabPanel(title = "Unique Occurence", icon = icon("database", "font-awesome"),
                                         DT::DTOutput("unique_data", height = "50%")),
                                tabPanel(title = "Duplicate Occurence", icon = icon("clone", "font-awesome"),
                                         DT::DTOutput("duplicate_data")),
                                tabPanel(title = "Geographic", icon = icon("globe", "font-awesome"),
                                         plotlyOutput("geo_distribution", height = "600px")
                                )
                              )
              ),
              column(4,
                     hr(),
                     checkboxInput("import_data_check", "Import data", value = FALSE),
                     conditionalPanel("input.import_data_check == true",
                                      selectInput("occ_type", "Data type",
                                                  choices = c("Presence", "Presence - Absence"),
                                                  selected = "Presence"),
                                      shinyFilesButton("choose_data_file", "Load data",
                                                       "Select Species occurrence data", multiple = FALSE,
                                                       icon = icon("file-upload")),
                                      actionButton("area_calibration", "Calibrate area",
                                                   icon = icon("draw-polygon"), style = bttn_primary_style)),
                     hr(),
                     conditionalPanel("output.geo_distribution != null",
                                      shinyFilesButton("add_layer", "Add layer",
                                                       title = "Select a vector file",
                                                       icon = icon("plus"),
                                                       multiple = FALSE)
                     ),

              )
              )
            )
            ),
    ## Predictors menu -----
    tabItem("predictors",
            fluidPage(
              fluidRow(column(8,
                              tabsetPanel(
                                tabPanel("Summary",
                                         verbatimTextOutput("pred_summary")),
                                tabPanel("Plot", icon = icon("image"),
                                         plotOutput("pred_plot")),
                                tabPanel("Colinearity", icon = icon("line"),
                                         shiny::div(
                                           DT::dataTableOutput("cr_df"),
                                           fluidRow(column(7, verbatimTextOutput("cr_layer")),
                                                    column(4, verbatimTextOutput("cr_remove")))
                                         )
                                )
                              )),
                       column(4,
                              hr(),
                              shinyDirButton("pred_source", "Source", icon = icon("folder"), style = bttn_primary_style,
                                             title = "Select folder containing predictors"),
                              hr(),
                              selectInput("pred_single", "Predictors", choices = c()),
                              fluidRow(column(3, actionButton("pred_load", "Show", icon = icon("eye"), style = bttn_primary_style)),
                                       column(4, actionButton("colinearize", "Colinearize", icon = icon("poll-h"), style = bttn_primary_style)),
                                       column(3, actionButton("occ_filt", "", icon = icon("braille"), style = bttn_second_style))),
                              tags$hr(),
                              tags$h4("Reduce colinearity"),
                              selectInput("coli_method", "Method", choices = colinearity_method),
                              conditionalPanel("input.coli_method == 'pearson'",
                                               numericInput("pearson_threshold", "Threshold", value = 0.8, min = 0, step = 0.1)),
                              conditionalPanel("input.coli_method == 'vif'",
                                               numericInput("vif_threshold", "Threshold", value = 10, min = 1)),
                              fluidRow(column(7, actionButton("reduce_collin", "Reduce colinearity",
                                                              icon = icon("sort-amount-down"), style = bttn_second_style))
                              )
                       ))
            )
            ),
    ## Data partition menu -----
    tabItem("data_partition",
            fluidPage(
              fluidRow(column(8,
                              hr(),
                              verbatimTextOutput("part_out"),
                              conditionalPanel("input.divvy_data > 0 &&
                                   (input.partition_type == 'part_sband' || input.partition_type == 'part_sblock')",
                                               plotOutput("part_sband_plot"))
              ),
              column(4,
                     hr(),
                     selectInput("partition_type", "Partitioning type", choices = partition_types),
                     conditionalPanel("input.partition_type == 'part_random'",
                                      selectInput("part_random_method", "Method",
                                                  choices = c("kfold", "rep_kfold", "loocv", "boot")),
                                      conditionalPanel("input.part_random_method == 'kfold' | input.part_random_method == 'rep_kfold'",
                                                       numericInput("kfold_number", "Number of folds", value = 10, min = 1, step = 1)),
                                      conditionalPanel("input.part_random_method == 'rep_kfold'",
                                                       numericInput("rep_kfold", "Number of replicates", value = 10, min = 1, step = 1)),
                                      conditionalPanel("input.part_random_method == 'boot'",
                                                       numericInput("rep_boot", "Number of replicates", value = 2, min = 1, step = 1),
                                                       numericInput("prop_boot", "Proportion", value = 0.7, min = 0, step = 0.1, max = 1)
                                      )
                     ),
                     conditionalPanel("input.partition_type == 'part_sband'",
                                      selectInput("sband_lon_lat", "Across", choices = c("Longitude" = "lon", "Latitude" = "lat")),
                                      numericInput("sband_part_number", "Number of partition", value = 2, min = 2, step = 1),
                                      fluidRow(column(6, numericInput("sband_part_min", "Min bands", value = 2, min = 2, step = 1, width = "55%")),
                                               column(6, numericInput("sband_part_max", "Max bands", value = 20, min = 1, step = 1, width = "55%"))),
                                      numericInput("sband_part_min_occ", "Min occurence", value = 10, min = 1, step = 1),
                                      numericInput("sband_part_prop", "Proportion of points", value = 0.5, min = 0, max = 1, step = 0.1)
                     ),
                     conditionalPanel("input.partition_type == 'part_sblock'",
                                      numericInput("sblock_part_number", "Number of partition", value = 2, min = 2, step = 1),
                                      fluidRow(column(6, numericInput("min_res_mult", "Min precision", value = 3, min = 2, step = 1, width = "55%")),
                                               column(6, numericInput("max_res_mult", "Max precision", value = 200, min = 2, step = 1, width = "55%"))),
                                      numericInput("num_grids", "Number of grid", value = 30, min = 1, step = 1),
                                      numericInput("sblock_min_occ", "Min occurence", value = 10, min = 1, step = 1),
                                      numericInput("sblock_prop", "Proportion of points", value = 0.5, min = 0, max = 1, step = 0.1)
                     ),
                     conditionalPanel("input.partition_type == 'part_senv'",
                                      fluidRow(column(6, numericInput("min_n_groups", "Min groups", value = 2, min = 1, step = 1, width = "55%")),
                                               column(6, numericInput("max_n_groups", "Max groups", value = 10, min = 1, width = "55%"))
                                      ),
                                      numericInput("senv_min_occ", "Min occurence", value = 10, min = 1),
                                      numericInput("senv_prop", "Proportion of points", value = 0.5, min = 0, max = 1, step = 0.1)
                     ),
                     fluidRow(column(4, actionButton("divvy_data", "Divvy", icon = icon("project-diagram"), style = bttn_primary_style)),
                              column(4, actionButton("valided_dp", "Validate", icon = icon("check-circle"), style = bttn_primary_style))
                     ),
                     hr(),
                     conditionalPanel("input.occ_type == 'Presence'",
                                      div(id = "bpas_check",
                                          p("You must generate background or pseudo-absence points for the modeling goals"),
                                          div(actionButton("back_ps_ab_samp", "Sample", style = "border: 2px solid #fd7769"),
                                              style = "float: right; position:relative; margin: -10px 12px")
                                      ))
              )

              )
            )


            ),
    ## Data Extraction ------
    tabItem("data_extraction",
            fluidPage(
              fluidRow(column(8,
                              div(id = "extracted_data_container",
                                  DT::DTOutput("extracted_data"))),
                       column(4,
                              selectInput("extract_variables", label = "Predictors to use",
                                          choices = c(), multiple = T, selected = c()),
                              actionButton("extract_data", "Extract data", icon = icon("table"), style = bttn_primary_style),
                              shinySaveButton(id = "save_extracted_data", label = "Save",  title = "Save occurence data filtered",
                                              filename = "", filetype = list(CSV = "csv", `Plain text` = "txt"), icon = icon("save"))
                       )
              )
            )
            ),
    ## MODELING
    ## Model fiting ----
    tabItem("model_fiting",
            fluidPage(
              fluidRow(column(6,
                              selectInput("fit_model_algorithm", label = "Algorithm", choices = c(),
                                          multiple = T, selected = c(), width = "100%")),
                       column(2, br(), checkboxInput("use_existing_df4mod", "Use existing data")),
                       column(2, br(), conditionalPanel("input.use_existing_df4mod == true",
                                                        shinyFilesButton("import_exiting_df4mod", "Load data",
                                                                         title = "Import data for modeling",
                                                                         icon = icon("upload"), multiple = F))
                       ),
                       column(2, br(), conditionalPanel("input.import_exiting_df4mod",
                                                        checkboxInput("esm", "Ensemble of Small Models"))
                       )),
              uiOutput("dynamic_model_fitting")
            )
            #uiOutput("dynamic_esm_model_fitting")
            ),
    tabItem("model_ensembling",
            fluidPage(
              fluidRow(column(7,
                              DT::DTOutput("fitted_model_list_dt"),
                              hr(),
                              verbatimTextOutput("selected_mod_lenght"),
                              DT::DTOutput("ens_performance")
              ),
              column(5,
                     h4("Ensembling parameter"),
                     selectInput("ens_method", "Method", choices = c()),
                     selectInput("ens_thr", "Threshol", choices = thr, multiple = T),
                     conditionalPanel("input.ens_thr.includes('sensitivity')",
                                      numericInput("ens_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
                     conditionalPanel("input.ens_method.includes('meanw', 'meandsup', 'meanthr')",
                                      selectInput("ens_thr_model", "Model threshold", choices = thr, selected = "equal_sens_spec")),
                     conditionalPanel("input.ens_thr_model.includes('sensitivity')",
                                      numericInput("ens_sens_model", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
                     selectInput("ens_metric", "Metric", choices = c("SORENSEN", "JACCARD", "FPB", "TSS", "KAPPA", "AUC", "IMAE", "BOYCE"), selected = "TSS"),
                     actionButton("fit_ens", "Ensemble", style = bttn_primary_style)
              )
              )
            )
            ),
    tabItem("spatial_predict",
            fluidPage(
              fluidRow(column(8,
                              DT::DTOutput("st_fitted_model_list_dt"),
                              tags$hr(),
                              DT::DTOutput("es_fitted_model_list_dt"),
                              div(style = "height:400px",
                                  DT::DTOutput("model_perf_merged", height = "90%")),
                              tags$hr(),
                              shinySaveButton("save_model_perf_merged", "Export",
                                              title = "Save models performance table",
                                              filename = "model_performance", filetype = list(CSV = "csv", `Plain text` = "txt"), icon = icon("save"))
              ),
              column(4,
                     selectInput("model_category", "Model category",
                                 choices = c("Standard models", "Ensemble")),
                     selectInput("predict_thr", "Threshol", choices = thr, multiple = T),
                     conditionalPanel("input.predict_thr.includes('sensitivity')",
                                      numericInput("predict_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
                     shinyFilesButton("predict_area", label = "Add area",
                                      title = "Select spatial polygon to restrict prediction",
                                      icon = icon("plus"), multiple = F),
                     br(), br(),
                     selectInput("predict_clamp", "Clamp", choices = c("No" = FALSE, "Yes" = TRUE),
                                 selected = TRUE),
                     selectInput("predict_pred_type", label = "Type of response",
                                 c("Link" = "link", "Exponential" = "exponential", "Cloglog" = "cloglog", "Logistic" = "logistic"),
                                 selected = "Cloglog"),
                     actionButton("predict", "Predict", icon = icon("qrcode", lib = "glyphicon"), style = bttn_primary_style),
                     hr(),
                     actionButton("merge_model_perf", "Merge performance",
                                  icon = icon("resize-small", lib = "glyphicon"), style = bttn_primary_style)
              )
              )
            ),
            ),
    tabItem("extrapolation",
            fluidPage(
              fluidRow(column(8,
                              shinycssloaders::withSpinner(plotOutput("extrapo_raster"),
                                                           color = loader_color, type = loader_type)),
                       column(4,
                              numericInput("n_cores", "Number of cores", min = 1, step = 1, value = 1),
                              numericInput("aggreg_factor", "Aggregation factor", min = 1, step = 1, value = 1),
                              actionButton("extrapo_model", "Extrapolate", style = bttn_primary_style)))
            )),
    tabItem("overpredict_correct",
            fluidPage(
              fluidRow(
                column(8,
                       shinycssloaders::withSpinner(plotOutput("ov_p_raster"),
                                                    color = loader_color, type = loader_type)),
                column(4,
                       selectInput("ov_p_cont_suit", "Suitability", choices = c()),
                       selectInput("ov_p_method", "Methode",
                                   choices = c("Occurrences Based Restriction" = "obr",
                                               "Presence" = "pres",
                                               "Lower Quantile" = "lq",
                                               "Minimum Convex Polygon" = "mcp",
                                               "Buffered Minimum Convex Polygon" = "bmcp")),
                       selectInput("ov_p_thr", "Threshold", choices = thr),
                       conditionalPanel("input.ov_p_method == 'bmcp'",
                                        numericInput("ov_p_buffer", "Buffer (m)", min = 1, value = 1500)),
                       actionButton("ov_p_correct", "Correct", style = bttn_primary_style)
                       )
              )
            )),

    tabItem("gbif_access",
            tabsetPanel(
              tabPanel("Query",
                       fluidPage(
                         column(8,
                                shinycssloaders::withSpinner(
                                leafletOutput("occ_map", height = "80vh"),
                                color = loader_color, type = loader_type),
                                absolutePanel(bottom = 20, right = 0,
                                              tags$div(
                                                id = "gt", style = "background-color:#a8b8ea;
                                                border-radius:10px; padding: 8px 8px; width:60%; margin-right:10px",
                                                #conditionalPanel("input.acces_gbif_data",
                                                                 actionButton("clear_map", "Clear", icon = icon("trash", lib = "glyphicon"), style = bttn_primary_style),
                                                                 #hr()),
                                                checkboxInput("use_geom_gbif", "Use defined area"),
                                                conditionalPanel("input.use_geom_gbif",
                                                                 shinyFilesButton("location_filter", "Add area", "Choose external area",
                                                                                  multiple = F, icon = icon("upload")))
                                              )

                                )
                         ),
                         column(4,
                                span(
                                  style = "width:100%; display: flex;",
                                  selectInput("search_by", "", choices = gbif_q, selected = "scientificName"),
                                  textInput("species_input", "", placeholder = "Enter ...")
                                ),
                                shinyWidgets::pickerInput("species_suggestions", "Select species", choices = NULL, multiple = FALSE),

                                span(
                                  id = "date", style = "width:100%; display: flex;",
                                  shinyWidgets::airDatepickerInput("date_filter_from", "Date range",
                                                                   minView = "years", maxDate = Sys.Date(), multiple = TRUE,
                                                                   clearButton = TRUE, view = "years", value = ""),
                                ),
                                span(
                                  id = "location", style = "width:100%; display: flex;",
                                  selectInput("country_filter", "Country", choices = countries, selected = NULL)

                                ),

                                actionButton("acces_gbif_data", "Access data", icon = icon("data"), style = bttn_primary_style)
                         )
                       )
                       ),
              tabPanel("Occurence",
                       fluidPage(
                         tagList(
                           #actionButton("load_gbif_data", "Load", icon = icon("refresh", lib = "glyphicon"), style = bttn_second_style),
                           shinySaveButton(id = "export_occ", label = "Export", title = "Save occurrence data",
                                           filename = gsub("\\s", "_", paste0("occurrence")),
                                           filetype = list(CSV = "csv", `Plain text` = "txt"),
                                           icon = icon("save")),
                           actionButton("add_to_map", "Add to map", icon = icon("plus")),
                           ), hr(),
                         DT::DTOutput("occ_gbif_dataset", height = "500px", fill = FALSE)
                       )
              ),
              tabPanel("Citation",
                       fluidPage(
                         shiny::verbatimTextOutput("occ_citation"),
                       )
                       )
            )

            )

)
)


## UI ####
ui <- shinydashboardPlus::dashboardPage(
  #skin = 'blue-light',
  header = header,
  sidebar = sidebar,
  body = nimo_body,
  footer = NULL,
  options = list(sidebarExpandOnHover = TRUE),
  scrollToTop =TRUE,
  title = "nimo"
)
#
# skin = “blue”, “blue-light”, “black”, “black-light”, “purple”, “purple-light”,
# “green”, “green-light”, “red”, “red-light”, “yellow”, “yellow-light”, “midnight”
# selectInput("occ_database", "Database",
#             choices = c("GBIF" = "gbif",
#                         "iNaturalist" = "inat",
#                         "eBird" = "ebird",
#                         "VertNet" = "vertnet",
#                         "iDigBio" = "idigbio",
#                         "OBIS" = "obis",
#                         "ALA" = "ala"))
