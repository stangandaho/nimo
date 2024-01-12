# Load packages
src_root <- paste0(system.file("nimo", package = "nimo"), "/src/")#"./inst/nimo/src/"#
call_src <- function(file_name) { return(source(paste0(src_root, file_name)))}
suppressPackageStartupMessages( call_src("packages.R") )

# Threshold
thr <- c("No omission" = "lpt", "Sensitivity = specificity" = "equal_sens_spec",
         "TSS" = "max_sens_spec", "Jaccard" = "max_jaccard",
         "Sorensen" = "max_sorensen", "FPB" = "max_fpb",
         "Sensitivity" = "sensitivity")

# Metric
metric <- c("SORENSEN", "JACCARD", "FPB", "TSS", "KAPPA", "AUC", "IMAE", "BOYCE")

# Button style
bttn_primary_style <-  paste0("background-color:", "#065325;", "color:#ffffff;")
bttn_second_style <- paste0("background-color:#065325;", "color:#ffffff;", "hover:red")
bttn_third_style <- paste0("background-color:#B7C1C6")
bttn_warn <- paste0("background-color:#f0b0a9", "border-color:#f0b0a9")
loader_color <- "#042d0f"; loader_type  <- 7

# Web page link
git_repo <- "https://github.com/stangandaho/nimo"
git_issues <- "https://github.com/stangandaho/nimo/issues"
nimo_site <- "https://nimo.re-agro.org/"
get_started_url <- "https://nimo.re-agro.org/get-started/"
nimo_logo <- "nimo/nimo_logo.png"; gbif_white_logo <- "nimo/gbif_white_logo.png"
mytitle <-   tags$link(tags$a(href = nimo_site,
                              tags$img(src= nimo_logo, height = '32',width='37')),
                       strong("nimo", style = "font-size: 1.8em;     font-family: 'Montserrat-Bold';"))
# Colinearity methods
colinearity_method <- c("Pearson correlation" = "pearson", "Variance inflation factor" = "vif",
                        "Principal component analysis" = "pca", "Factorial analysis" = "fa")
partition_types <- c("Random" = "part_random", "Spatial band" = "part_sband",
                     "Spatial block" = "part_sblock", "Environmental and Spatial" = "part_senv")

## GBIF utilities
## Field to query
gbif_q <- c(
  "Scientific name" = "scientificName", "Phylum" = "phylum", "Order" = "order",
  "Class" = "class", "Genus" = "genus", "Family" = "family", "Status" = "status",
  "Rank" = "rank", "Parent" = "parent", "Higher classification map" = "higherClassificationMap",
  "Synonym" = "synonym", "Family key" = "familyKey", "Canonical name" = "canonicalName",
  "Key" = "key", "Name key" = "nameKey", "kingdom" = "kingdom",
  "Nub key" = "nubKey", "Phylum key" = "phylumKey", "Parent key" = "parentKey",
  "Genus key" = "genusKey", "Order key" = "orderKey", "Kingdom key" = "kingdomKey",
  "Taxon key" = "taxonKey"
)

## Set country iso code
cc <- paste0(system.file("", package = "nimo"))#"./inst"
ccode <- read.csv(paste0(cc, "/extdata/country_code.csv"), header = TRUE, sep = ",") %>%
  dplyr::select(country, iso) %>%
  dplyr::mutate(iso = dplyr::case_when(is.na(iso) ~ "NA",
                                       TRUE ~ iso))
ccode <- rbind(data.frame("country" = "Any country","iso" = ""), ccode)

countries <- ccode$iso
names(countries) <- ccode$country

#--- Copy citation ---
## --> OBJECT IMPORTED FROM shinyCopy2clipboard: https://github.com/deepanshu88/shinyCopy2clipboard/tree/main
usecopy <- function (){
  tagList(shiny::singleton(shiny::tags$head(shiny::tags$script(src = "nimo/clipboard.min.js"),
                                            shiny::tags$script(src = "nimo/copy2clipboard.js"))),
          shiny::tags$body(shiny::tags$script(src = "nimo/tooltip.js")))
}

##
copy_button <- function (id, label, text = "No Text", icon = NULL, width = NULL,
                         class = NULL, modal = FALSE, ...){
  shiny::actionButton(inputId = id, label = label, icon = icon,
                      width = width, class = class, `data-clipboard-text` = text,
                      onclick = if (!modal) {
                        paste0("new ClipboardJS(\".btn\", document.getElementById(\"",
                               id, "\"));")
                      }
                      else {
                        paste0("new ClipboardJS(\".btn\", { container: document.getElementById(\"",
                               id, "\") });")
                      }, ...)
}

## Session time out
session_time_out <- function() {
  shiny::singleton(shiny::tags$head(shiny::tags$script(src = "nimo/session_time_out.js")))
}

#--- Dropdown - Help page -----------
box_dropdown_item <- function (..., id = NULL, href = NULL, icon = NULL)
{
  shiny::tags$li(shiny::tags$a(id = id, class = if (!is.null(id))
    "action-button", href = if (!is.null(href))
      href
    else "#", target = if (!is.null(href))
      "", icon, ...))
}
# ---------


# HEADER ----
header <- shinydashboardPlus::dashboardHeader(title = mytitle,
                                              titleWidth = 300,
                                              dropdownMenu(
                                                type = "messages",
                                                icon = icon("question-sign", lib = "glyphicon"),
                                                badgeStatus = NULL,
                                                headerText = strong("Help", style = "font-family:Montserrat-Bold; font-size:2rem"),
                                                box_dropdown_item(
                                                  "Get started",
                                                  href = get_started_url,
                                                  icon = icon("menu-right", lib = "glyphicon")
                                                ),
                                                box_dropdown_item(
                                                  "Issues",
                                                  href = git_issues,
                                                  icon = icon("bug"),
                                                ),
                                                box_dropdown_item(
                                                  "About",
                                                  href = "#",
                                                  icon = icon("info"),
                                                )
                                              ),
                                              tags$li(class = "dropdown",
                                                       tags$style(".main-header .logo {height: 53px}")
                                              )
                                              )


# SIDE BAR ----
sidebar <- shinydashboardPlus::dashboardSidebar(
  useShinyjs(), usecopy(), session_time_out(),
  sidebarMenu(
    id="sidebar_menu",
    menuItem("Home", tabName = "nimo_home_page", icon = icon("house-chimney")),
    menuItem(
      text = tags$span(
        tags$img(src = gbif_white_logo, height = '26', width='75')),
        tabName = "gbif_access", icon = NULL),
    menuItem("Pre-Modeling",tabName = "pre_modeling", icon = icon("arrow-left", lib = "glyphicon"),
             br(),
             actionButton("set_directory", "Set Directory", icon = icon("folder")),
             menuSubItem("Calibration", tabName = "calibration", icon = NULL),
             menuSubItem("Predictors", tabName = "predictors", icon = NULL),
             menuSubItem("Partition", tabName = "data_partition", icon = NULL),
             menuSubItem("Data extraction", tabName = "data_extraction", icon = NULL)
             ),
    ## MODELING
    menuItem("Modeling", tabName = "sd_modeling", icon = icon("play", lib = "glyphicon"),
             menuSubItem("Fitting", tabName = "model_fiting", icon = NULL),
             menuSubItem("Ensembling", tabName = "model_ensembling", icon = NULL)
             ),
    menuItem("Post-Modeling", tabName = "post_modeling", icon = icon("arrow-right", lib = "glyphicon"),
             menuSubItem("Spatial predictions", tabName = "spatial_predict", icon = NULL),
             menuSubItem("Extrapolation ", tabName = "extrapolation", icon = NULL),
             menuSubItem("Overprediction correction", tabName = "overpredict_correct", icon = NULL)
             ),
    ## CONFIGURATION
    div(
      menuItem("", tabName = "configuration", icon = icon("wrench")),
      style = "position:absolute; bottom:0; left:0; right:0; margin:15px 15px;
    list-style-type: none"
    )
  ),
  textOutput("res"),
  width = 300
)


# BODY ----
nimo_body <- shinydashboard::dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "nimo/style.css"),
    tags$link(rel = "icon", type = "image/png", href = nimo_logo),
    tags$style(".scrolling-container {
      display: flex;
      flex-wrap: nowrap;
      overflow-x: scroll;
      width: 100%;}"),
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
            });')
    ),
  tabItems(
    #### Homepage menu ----
    tabItem("nimo_home_page",
            fluidPage(
              fluidRow(
                column(12,
                       div(id = "main_title",
                           h1("Species distribution modeling (SDM) App",
                              style="text-align:center; font-size:50px;"))
                ),
                column(12,
                       div(shiny::img(src = nimo_logo, height = "25%",width = "30%"), style="text-align: center;")
                ),
                column(12,
                       div(p(strong("To start quickly with NIMO, please visite the start page "),
                             em(a("here",
                                  href = get_started_url)),
                             style="text-align:center; font-size:2.5rem; color:#00910a")),
                ),
                hr(),
                column(12,
                       div(id = "main_page",
                           div(h5("nimo is open-source and freely available for use, distributed under the MIT license.")),
                           # div(h5("When they are used in a publication, we ask that authors to cite the following reference:")),
                           # br(),
                           # div(h5("GANDAHO, S.M. . (2023).", strong(" nimo: ..."))),
                           # div(h5(strong("species distribution modeling."), em("Journal of ..."),", xx(6), xxx-xxx.")),
                           # br(),
                           # div(h5(strong("Failure to properly cite the software is considered a violation of the license🥺"),
                           #        style = "color:red;"))
                           )
                )

              )
            )
    ),
    ## Calibration menu ----
    tabItem("calibration",
            fluidPage(
              fluidRow(column(8,
                              tabsetPanel(
                                tabPanel(title = "Unique Occurrence", icon = icon("database", "font-awesome"),
                                         DT::DTOutput("unique_data", height = "50%")),
                                tabPanel(title = "Duplicate Occurrence", icon = icon("clone", "font-awesome"),
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
                                                       icon = icon("file-upload"))
                                      ),
                     hr(),
                     conditionalPanel("output.geo_distribution != null",
                                      shinyFilesButton("add_layer", "Add layer",
                                                       title = "Select a vector file",
                                                       icon = icon("plus"),
                                                       multiple = FALSE, style = bttn_primary_style),
                                      actionButton("area_calibration", "Calibrate area",
                                                   icon = icon("draw-polygon"), style = bttn_primary_style)
                     )

              )
              )
            )
            ),
    ## Predictors menu ----
    tabItem("predictors",
            fluidPage(
              fluidRow(
                column(8,
                       tabsetPanel(
                         tabPanel("Summary", verbatimTextOutput("pred_summary")),
                         tabPanel("Plot",
                                  shinycssloaders::withSpinner(plotOutput("pred_plot"),
                                                                      color = loader_color, type = loader_type)),
                         tabPanel("Colinearity",
                                         shiny::div(
                                           DT::dataTableOutput("cr_df"),
                                           fluidRow(column(7, verbatimTextOutput("cr_layer")),
                                                    column(4, verbatimTextOutput("cr_remove"))
                                                    )
                                         )
                                )
                              )),
              column(4,
                      hr(),
                      shinyDirButton("pred_source", "Predictor Folder", icon = icon("folder"), style = bttn_primary_style,
                                     title = "Select folder containing predictors"),
                      hr(),
                      selectInput("pred_single", "Predictors", choices = c()),
                      fluidRow(column(3, actionButton("pred_load", "Show", icon = icon("eye"), style = bttn_primary_style)),
                               column(4, actionButton("colinearize", "Colinearize", icon = icon("poll-h"), style = bttn_primary_style)),
                               column(3, actionButton("occ_filt", "", icon = icon("braille"), style = bttn_second_style))),
                      tags$hr(),
                     shiny::div(
                       shiny::div(
                         tags$h5("Reduce colinearity"),
                         selectInput("coli_method", "Method", choices = colinearity_method),
                         conditionalPanel("input.coli_method == 'pearson'",
                                          numericInput("pearson_threshold", "Threshold", value = 0.7, min = 0, step = 0.1, max = 1)),
                         conditionalPanel("input.coli_method == 'vif'",
                                          numericInput("vif_threshold", "Threshold", value = 10, min = 1)),
                         fluidRow(column(7, actionButton("reduce_collin", "Reduce colinearity",
                                                         icon = icon("sort-amount-down"), style = bttn_second_style)))
                       ), style = "background-color:#fcfcfa; border-radius: 8px 8px; padding: 15px 15px"
                      )
                       )
              )
            )
            ),
    ## Data partition menu ----
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
                                                  choices = c("k-fold" = "kfold",
                                                              "Repeated k-fold" = "rep_kfold",
                                                              "Leave-One-Out CV" = "loocv",
                                                              "Bootstrap" = "boot")),
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
                                      numericInput("sband_part_min_occ", "Min occurrence", value = 10, min = 1, step = 1),
                                      numericInput("sband_part_prop", "Proportion of points", value = 0.5, min = 0, max = 1, step = 0.1)
                     ),
                     conditionalPanel("input.partition_type == 'part_sblock'",
                                      numericInput("sblock_part_number", "Number of partition", value = 2, min = 2, step = 1),
                                      fluidRow(column(6, numericInput("min_res_mult", "Min precision", value = 3, min = 2, step = 1, width = "55%")),
                                               column(6, numericInput("max_res_mult", "Max precision", value = 200, min = 2, step = 1, width = "55%"))),
                                      numericInput("num_grids", "Number of grid", value = 30, min = 1, step = 1),
                                      numericInput("sblock_min_occ", "Min occurrence", value = 10, min = 1, step = 1),
                                      numericInput("sblock_prop", "Proportion of points", value = 0.5, min = 0, max = 1, step = 0.1)
                     ),
                     conditionalPanel("input.partition_type == 'part_senv'",
                                      fluidRow(column(6, numericInput("min_n_groups", "Min groups", value = 2, min = 1, step = 1, width = "55%")),
                                               column(6, numericInput("max_n_groups", "Max groups", value = 10, min = 1, width = "55%"))
                                      ),
                                      numericInput("senv_min_occ", "Min occurrence", value = 10, min = 1),
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
    ## Data Extraction ----
    tabItem("data_extraction",
            fluidPage(
              fluidRow(column(8,
                              div(id = "extracted_data_container",
                                  DT::DTOutput("extracted_data"))),
                       column(4,
                              selectInput("extract_variables", label = "Predictors to use",
                                          choices = c(), multiple = TRUE, selected = c()),
                              actionButton("extract_data", "Extract data", icon = icon("table"), style = bttn_primary_style),
                              shinySaveButton(id = "save_extracted_data", label = "Save",  title = "Save occurrence data filtered",
                                              filename = "",
                                              filetype = list(CSV = "csv", `Plain text` = "txt"),
                                              icon = icon("save"))
                       )
              )
            )
            ),
    ## MODELING
    ## Model fiting ----
    tabItem("model_fiting",
              fluidRow(
                column(6,
                       selectInput("fit_model_algorithm", label = "Algorithm", choices = c(),
                                   multiple = TRUE, selected = c(), width = "100%")),
                column(2,
                       tagList(checkboxInput("esm", "ESM"))
                ),
                column(2,
                       checkboxInput("tuning", "Tune")),
                column(2,
                       shinyFilesButton("import_exiting_df4mod", "Load data",
                                        title = "Import data for modeling",
                                        icon = icon("upload"), multiple = FALSE))
                       ),
              uiOutput("dynamic_model_fitting")
            ),
    tabItem("model_ensembling",
            fluidPage(
              fluidRow(column(7,
                              DT::DTOutput("fml_st"),
                              hr(),
                              verbatimTextOutput("selected_mod_lenght"),
                              DT::DTOutput("ens_performance"),
              ),
              column(5,
                     selectInput("ens_method", "Method", choices = c()),
                     selectInput("ens_thr", "Threshol", choices = thr, multiple = TRUE),
                     conditionalPanel("input.ens_thr.includes('sensitivity')",
                                      numericInput("ens_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
                     conditionalPanel("input.ens_method.includes('meanw', 'meandsup', 'meanthr')",
                                      selectInput("ens_thr_model", "Model threshold", choices = thr, selected = "equal_sens_spec")),
                     conditionalPanel("input.ens_thr_model.includes('sensitivity')",
                                      numericInput("ens_sens_model", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
                     selectInput("ens_metric", "Metric", choices = metric, selected = "TSS"),
                     tagList(
                       actionButton("fit_ens", "Assemble", style = bttn_primary_style),
                       shiny::downloadButton("export_ens_table", "Download")
                     )
              )
              )
            )
            ),
    tabItem("spatial_predict",
            fluidPage(
              fluidRow(column(8,
                              DT::DTOutput("st_fitted_model_list_dt"),
                              #tags$hr(),
                              DT::DTOutput("esm_fitted_model_list_dt"),
                              div(style = "height:400px",
                                  DT::DTOutput("model_perf_merged", height = "90%")),
                              tags$hr()
              ),
              column(4,
                     selectInput("model_category", "Model category",
                                 choices = c("Standard models", "Ensemble")),
                     selectInput("predict_thr", "Threshol", choices = thr, multiple = TRUE),
                     conditionalPanel("input.predict_thr.includes('sensitivity')",
                                      numericInput("predict_sens", label = "Sensitivity value", value = 0.9, min = 0, max = 1, step = 0.01)),
                     shinyFilesButton("predict_area", label = "Add area",
                                      title = "Select spatial polygon to restrict prediction",
                                      icon = icon("plus"), multiple = FALSE),
                     br(), br(),
                     selectInput("predict_clamp", "Clamp", choices = c("No" = FALSE, "Yes" = TRUE),
                                 selected = TRUE),
                     selectInput("predict_pred_type", label = "Type of response",
                                 c("Link" = "link", "Exponential" = "exponential", "Cloglog" = "cloglog", "Logistic" = "logistic"),
                                 selected = "Cloglog"),
                     actionButton("predict", "Predict", icon = icon("qrcode", lib = "glyphicon"), style = bttn_primary_style),
                     hr(),
                     tagList(
                       actionButton("merge_model_perf", "Merge performance",
                                    icon = icon("resize-small", lib = "glyphicon"),
                                    style = bttn_primary_style),
                       shinySaveButton("save_model_perf_merged", "Export",
                                       title = "Save models performance table",
                                       filename = "model_performance",
                                       filetype = list(CSV = "csv", `Plain text` = "txt"),
                                       icon = icon("save"))
                     )
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
                              selectInput("extrap_metric", "Metric",
                                          choices = c("Mahalanobis" = "mahalanobis", "Euclidean" = "euclidean"),
                                          selected = "Mahalanobis"),
                              numericInput("n_cores", "Number of cores", min = 1, step = 1, value = 1),
                              numericInput("aggreg_factor", "Aggregation factor", min = 1, step = 1, value = 1),
                              tagList(
                                actionButton("extrapo_model", "Extrapolate", style = bttn_primary_style),
                                downloadButton("download_extrapo_raster", "Save", icon = icon("save"))
                              )
                              ))
            )),
    tabItem("overpredict_correct",
            fluidPage(
              fluidRow(
                column(8,
                       shinycssloaders::withSpinner(plotOutput("ov_p_raster"),
                                                    color = loader_color, type = loader_type)),
                column(4,
                       selectInput("ov_p_cont_suit", "Suitability", choices = c()),
                       tagList(
                         selectInput("ov_p_lon", "Longitude", choices = c(), width = "60%"),
                         selectInput("ov_p_lat", "Latitude", choices = c(), width = "60%")
                       ),
                       selectInput("ov_p_method", "Method",
                                   choices = c("Occurrences Based Restriction" = "obr",
                                               "Presence" = "pres",
                                               "Lower Quantile" = "lq",
                                               "Minimum Convex Polygon" = "mcp",
                                               "Buffered Minimum Convex Polygon" = "bmcp")),
                       selectInput("ov_p_thr", "Threshold", choices = thr),
                       conditionalPanel("input.ov_p_method == 'bmcp'",
                                        numericInput("ov_p_buffer", "Buffer (m)", min = 1, value = 1500)),
                       tagList(
                         actionButton("ov_p_correct", "Correct", style = bttn_primary_style),
                         actionButton("id_download_ov_p_correct", label = "Download",
                                      icon = shiny::icon("download"))
                       )
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
                                                id = "gt", style = "background-color:#b7c1c6;
                                                border-radius:10px; padding: 5px 5px; width:50%; margin-right:10px; margin-bottom:10px",
                                                                 actionButton("clear_map", "Clear", icon = icon("trash", lib = "glyphicon"), style = bttn_primary_style),
                                                checkboxInput("use_geom_gbif", "Use defined area"),
                                                conditionalPanel("input.use_geom_gbif",
                                                                 shinyFilesButton("location_filter", "Add area", "Choose external area",
                                                                                  multiple = FALSE, icon = icon("upload")))
                                              )

                                )
                         ),
                         column(4,
                                span(
                                  style = "width:100%; display: flex;",
                                  selectInput("search_by", "", choices = sort(gbif_q), selected = "scientificName"),
                                  textInput("species_input", "", placeholder = "Enter ...")
                                ),
                                shinyWidgets::pickerInput("species_suggestions", "Select species", choices = NULL, multiple = FALSE),

                                span(
                                  id = "date", style = "width:100%; display: flex;",
                                  shinyWidgets::airDatepickerInput("date_filter_from", "Date range",
                                                                   minView = "years", maxDate = Sys.Date(), multiple = TRUE,
                                                                   clearButton = TRUE, view = "years", value = "",
                                                                   dateFormat = "yyyy", addon = "none"),
                                ),
                                span(
                                  id = "location", style = "width:100%; display: flex;",
                                  selectInput("country_filter", "Country", choices = countries, selected = "")

                                ),
                                actionButton("acces_gbif_data", "Access data", icon = NULL, style = bttn_primary_style)
                         )
                       )
                       ),
              tabPanel("Occurrence",
                       fluidPage(
                         div(
                           shiny::downloadButton("export_occ2",  "Export", icon = shiny::icon("save"),
                                                 style = bttn_primary_style),
                           style = "margin-bottom: 15px; display: flex; justify-content: flex-end;"
                         ),
                         DT::DTOutput("occ_gbif_dataset", height = "500px", fill = FALSE)
                       )
              ),
              tabPanel("Citation",
                       fluidPage(
                         shiny::verbatimTextOutput("occ_citation", placeholder = TRUE),
                         tagList(
                           copy_button(
                             "copy_citation_btn",
                             "Copy",
                             icon = icon("copy"),
                             text = "No citation copied"
                           ),
                           shinyFiles::shinySaveButton("save_citation", "Save ciatation", title = "Save citation",
                                                       filename = "", filetype = list(`Plain text` = "txt"),
                                                       icon = icon("save"))
                         )
                       )
                       )
            )

            ),

    # Congiguration
    tabItem("configuration",
            fluidPage(
              shinyWidgets::panel(
                tagList(numericInput("set_seed", "Set seed", value = 123, min = 0, step = 1),
                        numericInput("sys_timeout", "Timeout (min)", value = 5, min = 1, step = 0.1),
                        actionButton("save_config", "Save change", style = bttn_primary_style))
              )
            )
    )

)
)

## UI ----
ui <- shinydashboardPlus::dashboardPage(
  header = header,
  sidebar = sidebar,
  body = nimo_body,
  footer = NULL,
  options = list(sidebarExpandOnHover = TRUE),
  scrollToTop =TRUE,
  title = "nimo"
)
