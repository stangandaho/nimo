## SERVER
server <- function(input, output, session) {
  src_root <- paste0(system.file("nimo", package = "nimo"), "/src/")#"./inst/nimo/src/"

  source(paste0(src_root, "global_variables.R"), verbose = FALSE, local = TRUE)$value
  source(paste0(src_root, "cust_functions.R"), verbose = FALSE, local = TRUE)$value
  source(paste0(src_root, "script_recorder.R"), verbose = FALSE, local = TRUE)$value
  source(paste0(src_root, "predictors_selection_update.R"), verbose = FALSE, local = TRUE)$value
  source(paste0(src_root, "render_model_fitting.R"), local = TRUE)[1]
  source(paste0(src_root, "model_fitting.R"), verbose = FALSE, local = TRUE)$value
  source(paste0(src_root, "model_tuning.R"), verbose = FALSE, local = TRUE)$value
  source(paste0(src_root, "esm_model_fitting.R"), verbose = FALSE, local = TRUE)$value
  source(paste0(src_root, "var_importance.R"), verbose = FALSE, local = TRUE)$value
  source(paste0(src_root, "response_curves.R"), verbose = FALSE, local = TRUE)$value
  source(paste0(src_root, "outliers.R"), verbose = FALSE, local = TRUE)$value
  source(paste0(src_root, "constraints.R"), verbose = FALSE, local = TRUE)$value
  source(paste0(src_root, "uncertainty.R"), verbose = FALSE, local = TRUE)$value
  source(paste0(src_root, "handle_sf.R"), verbose = FALSE, local = TRUE)$value
  source(paste0(src_root, "leaflet_map.R"), verbose = FALSE, local = TRUE)$value
  source(paste0(src_root, "query_gbif_occ_data.R"), verbose = FALSE, local = TRUE)$value

  # DIRECTORY SET UP ----
  set_dir_modal <- function() {
    modalDialog(
      title = strong("Project Directory Setup", style = "font-family:'Montserrat-Bold'"), size = "l",
      footer = tagList(
        modalButton("Ok"),
        actionButton("dir_setup", "Set up", icon("wrench"), style = bttn_primary_style)
        ),
      fluidRow(column(6, shinyFiles::shinyDirButton("browse_dir", "Directory",
                                                    title = "Project Directory",
                                                    icon = icon("folder"))),
               column(6, checkboxInput("cali_area", "Calibrate area", value = TRUE))),
      fluidRow(column(6, selectInput("algorithm", "Algorithm", choices = algorithm, multiple = TRUE, selected = "raf")),
               column(6, selectInput("ensemble", "Ensemble", choices = ensemble, selected = "mean"))),
      verbatimTextOutput("dir_setup_success")
    )
  }
  ## Show modal when button is clicked.
  observeEvent(input$set_directory, {
    showModal(set_dir_modal())
  })

  ## Create Project Directory
  root <- shinyFiles::getVolumes()()
  shinyFiles::shinyDirChoose(input, "browse_dir", roots = root)
  folder_path <- reactive({
    unname(shinyFiles::parseDirPath(roots = root, selection = input$browse_dir))
  })
  ## dir_setup invoking function
  project_dir <- eventReactive(input$dir_setup, {
    setwd(folder_path())# set working directory
    run_recorded(
      "setup", "setup", "", "project_dir", "flexsdm::sdm_directory", sdm_directory,
      list(main_dir = folder_path(),
           projections = "proj",
           calibration_area = isTRUE(input$cali_area),
           algorithm = input$algorithm,
           ensemble = input$ensemble,
           threshold = TRUE),
      pre = paste0("setwd(", nm_lit(folder_path()), ")")
    )
  })
  output$dir_setup_success <- renderText(paste("\n", {project_dir()}))

  # IMPORT DATA ---------
  ## set data file path
  shinyFiles::shinyFileChoose(input, "choose_data_file", roots = root,
                              filetypes = c("txt", "csv"))
  data_file_path <- reactive({
    unname(shinyFiles::parseFilePaths(roots = root, selection = input$choose_data_file)$datapath)
  })
  ## import data
  species_data <- reactive({
    #req(input$import_data_check)  # Ensure input is available

    if (input$import_data_check) {
      req(data_file_path())
      data <- run_recorded("occ_raw", "occurrences", "Occurrences imported from file",
                           "occ_raw", "read.csv", read.csv,
                           list(file = data_file_path(), header = TRUE))
    } else if (isTRUE(input$start_modeling > 0)) {
      data <- gbif_data()[[1]]
      record_step("occ_raw", "occurrences", "Occurrences queried from GBIF",
                  isolate(gbif_query_code()))
    } else {
      data <- NULL  # or return(NULL) if you want to suppress reactivity
    }

    data
  })

  ## set Modal to customize data importing
  data_customize_modal <- function(){
    sdm_data <- species_data()
    modalDialog(title = h3("Characterize the species data", style = "text-align:left"), size = "l",
                footer = tagList(
                  modalButton("Ok"),
                  actionButton("valid_data", "Valid", icon("check"), style = bttn_primary_style)
                  ),
                fluidRow(
                  column(6, selectInput("species_var", "Species column",
                                        choices = colnames(sdm_data))),
                  column(6, selectInput("unique_species", "Species",
                                        choices = c(), multiple = FALSE))
                ),
                fluidRow(
                  column(4, selectInput("long_var", "Longitude", choices = colnames(sdm_data))),
                  column(4, selectInput("lat_var", "Latitude", choices = colnames(sdm_data))),
                  column(4, selectInput("var_to_conserve", "Variable(s) to conserve",
                                        choices = c(), selected = c(), multiple = TRUE))
                ),
                fluidRow(
                  conditionalPanel("input.occ_type != 'Presence'",
                                   column(4, selectInput("occ_var", "Occurrence column", choices = colnames(sdm_data))),
                                   column(4, selectInput("presence", "Presence", choices = c())),
                                   column(4, selectInput("abscence", "Absence", choices = c())))
                )

                )
  }

  error_modal <- function(){
    modalDialog(title = h3("Error", style = "color:#c52323"),
                textOutput("error_modal"), footer = modalButton("Ok"))
  }
  ## import data
  observeEvent(input$choose_data_file, {
    req(data_file_path())
    tryCatch({
      if(is.data.frame(species_data())){
      showModal(data_customize_modal())}},
      error = function(e){
        showModal(error_modal())
        output$error_modal <- renderText(paste(e))
      })
    })
  ### update unique_species selection
  observeEvent(input$species_var, {
    tryCatch({
      updateSelectInput(session, "unique_species",
                        choices = unique(species_data()[, input$species_var]),
                        selected = unique(species_data()[, input$species_var])[1])
    }, error = error)
  })
  observeEvent(input$occ_var, {
    tryCatch({
      updateSelectInput(session, "presence",
                        choices = unique(species_data()[, input$occ_var]))
      updateSelectInput(session, "abscence",
                        choices = unique(species_data()[, input$occ_var]))
      updateSelectInput(session, "var_to_conserve",
                        choices = colnames(species_data()))
    }, error = error)
  })
  ## wrangle data imported -----
  wrangle_data <- eventReactive(input$valid_data, {
    req(species_data(), input$species_var, input$long_var, input$lat_var)
    lit <- nm_lit
    sp <- input$species_var; lon <- input$long_var; lat <- input$lat_var
    not_na_xy <- paste0("  dplyr::filter(!is.na(.data[[", lit(lon), "]]), !is.na(.data[[", lit(lat), "]]))")
    if (input$occ_type != "Presence") {
      code <- c(
        "occ_sel <- occ_raw %>%",
        paste0("  dplyr::select(dplyr::all_of(",
               lit(unique(c(sp, lon, lat, input$occ_var, input$var_to_conserve))), ")) %>%"),
        paste0("  dplyr::filter(.data[[", lit(sp), "]] %in% ", lit(input$unique_species), ","),
        paste0("                .data[[", lit(input$occ_var), "]] %in% ",
               lit(c(input$presence, input$abscence)), ") %>%"),
        paste0("  dplyr::mutate(uni = paste0(.data[[", lit(lon), "]], .data[[", lit(lat), "]]))"),
        "occ <- occ_sel %>%",
        "  dplyr::distinct(uni, .keep_all = TRUE) %>%",
        "  dplyr::select(-uni) %>%",
        paste0("  dplyr::rename(pr_ab = ", lit(input$occ_var), ") %>%"),
        "  dplyr::filter(!is.na(pr_ab)) %>%",
        paste0("  dplyr::mutate(pr_ab = dplyr::case_when(pr_ab == ", lit(input$presence), " ~ 1,"),
        paste0("                                         pr_ab == ", lit(input$abscence), " ~ 0)) %>%"),
        not_na_xy
      )
    } else {
      code <- c(
        "occ_sel <- occ_raw %>%",
        paste0("  dplyr::select(dplyr::all_of(", lit(unique(c(sp, lon, lat, input$var_to_conserve))), ")) %>%"),
        paste0("  dplyr::filter(.data[[", lit(sp), "]] == ", lit(input$unique_species), ") %>%"),
        paste0("  dplyr::mutate(uni = paste0(.data[[", lit(lon), "]], .data[[", lit(lat), "]]))"),
        "occ <- occ_sel %>%",
        "  dplyr::distinct(uni, .keep_all = TRUE) %>%",
        "  dplyr::select(-uni) %>%",
        "  dplyr::mutate(pr_ab = 1,",
        paste0("                dplyr::across(dplyr::all_of(", lit(c(lon, lat)), "), as.numeric)) %>%"),
        not_na_xy
      )
    }
    code <- paste(c(
      code,
      "occ_dupl <- occ_sel %>% dplyr::filter(duplicated(uni)) %>% dplyr::select(-uni)",
      "occ_dupl <- occ_dupl[complete.cases(occ_dupl), ]",
      "rm(occ_sel)"
    ), collapse = "\n")
    env <- run_code("occ", "occurrences", "Species records cleaning", code,
                    list(occ_raw = species_data()))
    return(list(env$occ, env$occ_dupl))
  })
  # display data
  output$unique_data <- st_render_dt({ wrangle_data()[[1]] })
  output$duplicate_data <- st_render_dt({ wrangle_data()[[2]] })

  ## plot geographic distribution
  observeEvent(input$valid_data, {
    # point distribution
    wdt <- wrangle_data()[[1]] %>% mutate(pr_ab = as.character(pr_ab))
    occ_plot <- ggplot(data = wdt)+
      geom_point(aes(!!dplyr::sym(input$long_var), !!dplyr::sym(input$lat_var),
                     color = if(length(unique(pr_ab)) > 1) {pr_ab} ))+
      labs(x = "Longitude\n", y = "Latitude\n")+
      scale_color_manual(values = c("#878787", "#323232"))+
      guides(color = guide_legend(title = ""))

      output$geo_distribution <- renderPlotly({
        ggplotly(occ_plot, tooltip = c("x", "|", "y"), originalData = TRUE, dynamicTicks = TRUE)
      })

  })
  # IMPORT SHAPEFILE -------
  ## set layer files path
  shinyFiles::shinyFileChoose(input, "add_layer", roots = root,
                              filetypes = c("shp", "kml", "kmz"))
  layer_file_path <- reactive({
    unname(shinyFiles::parseFilePaths(roots = root, selection = input$add_layer)$datapath)
  })
  ## set shp data
  shp_layer <- reactive({
    req(layer_file_path())
    code <- paste0("study_area <- sf::read_sf(", nm_lit(layer_file_path()), ") %>%\n",
                   "  sf::st_make_valid(duplicate_edges = FALSE, split_crossing_edges = TRUE)")
    run_code("layer", "layer", "", code)$study_area
    })

  occ_data <- reactive({
    tryCatch({
      sf::st_as_sf(x = wrangle_data()[[1]],
                   coords = c(input$long_var, input$lat_var),
                   crs = sf::st_crs(shp_layer())) %>%
        mutate(pr_ab = as.character(pr_ab))
    }, error = error)
    })
  observe({
    req(shp_layer())
    if (sf::st_geometry_type(shp_layer(), F) != "POLYGON") {
      showModal(error_modal())
      output$error_modal <- renderText("The layer must be a polygon.")
    }
  })
  ### plot shp and occurrence data
  observeEvent(input$add_layer, {
    req(layer_file_path())
    occ_and_shp_layer <- ggplot()+
      geom_sf(data = shp_layer(), aes())+
      #geom_sf(data = occ_data(), aes(), color = "#f26802")+
      geom_sf(data = occ_data(), aes(color = if(length(unique(pr_ab)) > 1) {pr_ab} ))+
      labs(x = "Longitude\n", y = "Latitude\n")+
      scale_color_manual(values = c("#878787", "#323232"))+
      guides(color = guide_legend(title = ""))

    output$geo_distribution <- renderPlotly({
      ggplotly(occ_and_shp_layer, tooltip = c("x", "|", "y"), originalData = TRUE, dynamicTicks = TRUE)
    })
  })

  # CALIBRATE AREA ---------
  ### calibration modal
  calib_area_modal <- function(){
    modalDialog(h3("Area Calibration", style = "text-align:left"), size = "l",
                footer = tagList(
                                 modalButton("Ok"),
                                 actionButton("valid_calib_area", "Calibrate", style = bttn_primary_style)
                  ),
                fluidRow(
                  column(8, plotOutput("calib_area_plot")),
                  column(4,
                    strong(textOutput("da_la_unav"), style = "color:#c52323; text-align:justify"),
                    br(),
                    selectInput("calib_area_method", "Method", choices = calib_area_method),
                    conditionalPanel("input.calib_area_method == 'buffer' | input.calib_area_method == 'bmcp'",
                                     numericInput("calib_area_width", "Width (m)", value = 3000, min = 5, width = "100%")),
                    conditionalPanel("input.calib_area_method == 'mask'",
                                     selectInput("clusters_field", "Filter by", choices = c()))
                    )
                )
              )
  }
  observeEvent(input$area_calibration,{
    output$da_la_unav <- renderText(paste("Provide occurrence data and geographic delimitation layer\n
                                           to process to area calibration"))
  })
  observeEvent(input$area_calibration, {
    showModal(calib_area_modal())
  })
  observeEvent(input$area_calibration, {
      req(data_file_path(), wrangle_data()[[1]], layer_file_path())
      if(is.data.frame( wrangle_data()[[1]] ) & layer_file_path() != ""){
      showModal(calib_area_modal())
      hide("da_la_unav")}
  })

  ## update cluster filtering field for mask method
  observeEvent(input$area_calibration, {
    req(layer_file_path())
    updateSelectInput(session, "clusters_field",
                      choices = colnames(shp_layer())[colnames(shp_layer()) != "geometry"])
  })

  ## function to calibrate area
  # drop point outside area
  calib_res <- eventReactive(input$valid_calib_area, {
    req(shp_layer(), wrangle_data())
    lon <- nm_lit(input$long_var); lat <- nm_lit(input$lat_var)
    method <- switch(input$calib_area_method,
                     "buffer" = nm_lit(c("buffer", width = as.numeric(input$calib_area_width))),
                     "bmcp" = nm_lit(c("bmcp", width = as.numeric(input$calib_area_width))),
                     "mcp" = nm_lit(c("mcp")),
                     "mask" = paste0('c("mask", terra::vect(study_area), ', nm_lit(input$clusters_field), ')'))
    code <- paste(c(
      paste0("occ_in_area <- sf::st_as_sf(occ, coords = c(", lon, ", ", lat, "), crs = sf::st_crs(study_area)) %>%"),
      paste0("  dplyr::bind_cols(occ[, c(", lon, ", ", lat, ")]) %>%"),
      "  sf::st_intersection(study_area) %>%",
      "  as.data.frame() %>%",
      "  dplyr::select(-geometry)",
      "calib_area <- flexsdm::calib_area(",
      "  data = occ_in_area,",
      paste0("  x = ", lon, ","),
      paste0("  y = ", lat, ","),
      "  crs = terra::crs(study_area),",
      paste0("  method = ", method),
      ")"
    ), collapse = "\n")
    env <- run_code("calib_area", "calibration", "", code,
                    list(occ = occ_used(), study_area = shp_layer()))
    list(occ_dt = env$occ_in_area, calib_area = env$calib_area)
  })
  occ_dt <- function() calib_res()$occ_dt
  calib_area_nimo <- function() calib_res()$calib_area

  ## Notify calibration area valid
  observeEvent(input$valid_calib_area, {
    req(calib_area_nimo())
    if (class(calib_area_nimo()) == "SpatVector") {
      showNotification(
        id = "cala_validity_status",
        ui = "Area calibrated and saved",
        duration = 6,
        closeButton = TRUE,
        type = "message"
      )
    }
  })
  ## plot calibrated area
  plot_cali_area <- eventReactive(input$valid_calib_area, {
      req(calib_area_nimo(), layer_file_path(), wrangle_data(), shp_layer())
    occ_dt <- sf::st_as_sf(occ_dt(), coords = c(input$long_var, input$lat_var), crs = crs(shp_layer()))

      ##
      if (input$calib_area_method != "mask") {
        ca_plot <-  ggplot()+
          geom_sf(data = shp_layer())+
          geom_sf(data = sf::st_as_sf(calib_area_nimo()), fill = "#e5d9d9", color = "#947368")+
          geom_sf(data = occ_dt)+
          theme_void()
      } else {
        ca_plot <-  ggplot()+
          geom_sf(data = sf::st_as_sf(calib_area_nimo()), fill = "#e5d9d9", color = "#947368")+
          geom_sf(data = occ_dt)+
          theme_void()
      }
      return(ca_plot)
  })

  output$calib_area_plot <- renderPlot(plot_cali_area())

  # PREDICTORS -----
  shinyFiles::shinyDirChoose(input, "pred_source", roots = root,
                              filetypes = c("tif", "TIF", "tiff", "TIFF"))
  predictors_path <- reactive({
    unname(shinyFiles::parseDirPath(roots = root, selection = input$pred_source))
  })

  pred_list <- reactive({
    list.files(path = predictors_path(), pattern = "\\.tiff?$", ignore.case = TRUE, full.names = TRUE)
  })
  env_base <- reactive({
    req(pred_list())
    all_rast <- to_check <- n_col <- n_row <- name <- list(); rast_n <- 0
    for (rst_path in pred_list()) {
      rast_n <- rast_n + 1
      rast_tc <- terra::rast(rst_path)
      all_rast[[rast_n]] <- rast_tc
      to_check[[rast_n]] <- paste0(terra::ncol(rast_tc), terra::nrow(rast_tc))
      n_col[[rast_n]] <- terra::ncol(rast_tc)
      n_row[[rast_n]] <- terra::nrow(rast_tc)
      name[[rast_n]] <- terra::names(rast_tc)
    }
    to_check <- unlist(to_check)

    if (!all(to_check == to_check[1])) {
      showModal(
        shiny::modalDialog(title = h3("Predictor properties differ", style = "text-align:left"),
                           footer = modalButton("Ok"), size = "m",
                           shinycssloaders::withSpinner(DT::dataTableOutput("rast_properties_table"),
                                                        color = loader_color, type = loader_type),
                             br(),
                             textOutput("unmatched_properties_warning")
                           )
      )

      output$rast_properties_table <- DT::renderDT(
        data.frame(Predictor = unlist(name), Rows = unlist(n_row), Columns = unlist(n_col)),
        options = list(scrollX = TRUE, scrollY = TRUE, lengthChange = FALSE,
                       pageLength = 5, searching = FALSE),
        selection = "none", rownames = FALSE
      )
      output$unmatched_properties_warning <- renderText(paste0(
        "We matched all predictor properties (resolution and extend) to ", terra::names(all_rast[[1]]),
        "'s properties. Nearest neighbor method is used to estimate the new cell values."
      ))

      all_rast_sampled <- list()
      for (rst_ in 1:length(all_rast)) {
        tgt <- all_rast[[1]]
        tmch <- all_rast[[rst_]]
        tmch <- terra::project(tmch, tgt)
        all_rast_sampled[[rst_]] <- nm_match_raster(to_match = tmch, target = tgt,
                                                    method = 'near')
      }
      ras <- rast(all_rast_sampled)
      record_step("env", "predictors", "", c(
        env_files_code(),
        "# match all predictors to the first one",
        "env_target <- terra::rast(env_files[1])",
        "env <- terra::rast(lapply(env_files, function(f) {",
        "  r <- terra::project(terra::rast(f), env_target)",
        "  nimo::nm_match_raster(to_match = r, target = env_target, method = \"near\")",
        "}))"
      ))
      ras
    } else{
      tryCatch({
        ras <- terra::rast(pred_list())
        code <- c(env_files_code(), "env <- terra::rast(env_files)")
        if (base::any(duplicated(names(ras)))) {
          names(ras) <- sub("\\.[^.]*$", "", basename(pred_list()))
          code <- c(code, 'names(env) <- sub("\\\\.[^.]*$", "", basename(env_files))')
        }
        record_step("env", "predictors", "", code)
        ras
      }, error = function(e){
        showModal(error_modal())
        output$error_modal <- renderText(paste(e))
      })
    }
  })
  env_files_code <- function() {
    paste0("env_files <- list.files(", nm_lit(predictors_path()),
           ', pattern = "\\\\.tiff?$", ignore.case = TRUE, full.names = TRUE)')
  }
  ## display summary
  predictors_summary <- function() {
    pred_rast <- c()
    for (i in pred_list()) {
      ras <- terra::rast(i)
      pred_rast <- c(pred_rast, ras)
    }
    return(pred_rast)
  }
  output$pred_summary <- renderText(paste("No predictor loaded yet"))
  observeEvent(input$pred_source, {
  req(predictors_path())
  if (!is.null(input$pred_source) | predictors_path() != "") {
    output$pred_summary <- renderPrint(predictors_summary())
  }
  })
  ## plot predictor
  ## update single predictor selection

  observeEvent(input$pred_source, {
    updateSelectInput(session, inputId = "pred_single", choices = names(env_layers()))
  })
  pred_plot <- eventReactive(input$pred_load,{
    terra::plot(env_layers()[input$pred_single], main = input$pred_single)
  })
  output$pred_plot <- renderPlot(pred_plot())

  # REDUCE COLINEARITY -----
  observeEvent(input$colinearize, {
    req(pred_list())
    tryCatch({
      preds <- terra::rast(pred_list())
      showModal(colin_modal())
      output$colin_corr <- renderPlot(terra::pairs(preds))
    },
    error = function(e){
      showModal(error_modal())
      output$error_modal <- renderText(paste(e))
    })
  })
  colin_modal <- function(){
    modalDialog(title = h3("Pair plots of predictor", style = "text-align:left"),
                footer = modalButton("Ok"), size = "l",
                shinycssloaders::withSpinner(plotOutput("colin_corr"),
                                             color = loader_color, type = loader_type))
  }

  reduce_colin <- eventReactive(input$reduce_collin, {
    req(env_base())
    tryCatch({
      colin_method <- switch(input$coli_method,
                             "pearson" = c("pearson", th = as.character(input$pearson_threshold)),
                             "vif" = c("vif", th = as.character(input$vif_threshold)),
                             input$coli_method)
      colin_post <- switch(input$coli_method,
                           "pearson" = paste0("removed_vars <- nimo::nm_find_hcv(colin$cor_table, cutoff = ",
                                              nm_lit(input$pearson_threshold), ")"),
                           "pca" = "colin$cumulative_variance",
                           "removed_vars <- colin$removed_variables")
      reset_pca_use()
      correct_colinvar <- function(env_layer, method) {
        run_recorded("colin", "colinearity", "", "colin", "flexsdm::correct_colinvar",
                     flexsdm::correct_colinvar,
                     list(env_layer = nm_cv(env_layer, "env"), method = colin_method),
                     post = colin_post)
      }
      if(input$coli_method == "pearson"){
        colin_var <- correct_colinvar(env_layer = env_base(),
                                      method = c("pearson", th = as.character(input$pearson_threshold)))
        cr_df <- colin_var$cor_table # table
        enlayer <- colin_var$cor_variables # layer
        highly_corr_vars <- nm_find_hcv(cr_df, cutoff = input$pearson_threshold)
        rm_enlayer <- highly_corr_vars
      } else if(input$coli_method == "vif"){
        colin_var <- correct_colinvar(env_layer = env_base(), method = c("vif", th = as.character(input$vif_threshold)))
        cr_df <- colin_var$vif_table # table
        enlayer <- list()
        for (i in 1:terra::nlyr(colin_var$env_layer)) {
          l <- colin_var$env_layer[[i]]
          enlayer[[i]] <- l
        }
        rm_enlayer <- colin_var$removed_variables
      } else if(input$coli_method == "pca"){
        colin_var <- correct_colinvar(env_layer = env_base(), method = c("pca"))
        cr_df <- colin_var$coefficients # table
        enlayer <- list()
        for (i in 1:terra::nlyr(colin_var$env_layer)) {
          l <- colin_var$env_layer[[i]]
          enlayer[[i]] <- l
        }
        rm_enlayer <- colin_var$cumulative_variance
      } else if(input$coli_method == "fa" & terra::nlyr(env_base()) > 2){
        colin_var <- correct_colinvar(env_layer = env_base(), method = c("fa"))
        cr_df <- colin_var$loadings # table
        enlayer <- colin_var$env_layer
        rm_enlayer <- colin_var$removed_variables
      }

      return(list(cr_df, enlayer, rm_enlayer, colin_var))
    }, error = error)
  })

  cr_env <- eventReactive(input$reduce_collin, {
    switch(input$coli_method,
           "pearson" = cat("Pairwise relations that exceeded the correlation threshold for each one of the environmental variables:\n\n"),
           "vif" = cat("SpatRaster object with selected environmental variables:\n\n"),
           "pca" = cat("SpatRaster with scores of selected principal component (PC)\nthat sum up 95% of the whole variation or original environmental variables:\n\n"),
           "fa" = cat("Selected variables due to correlation to factors:\n\n"))
  })
  cr_rm_env <- eventReactive(input$reduce_collin, {
    switch(input$coli_method,
           "pearson" = cat("Removed predictor:\n\n"),
           "vif" = cat("Removed predictor:\n\n"),
           "pca" = cat("Cumulative variance explained in selected (PC):\n\n"),
           "fa" = cat("Removed predictor:\n\n"))
  })
  output$cr_df <- st_render_dt({reduce_colin()[[1]]})
  output$cr_layer <- renderPrint({
    cr_env()
    reduce_colin()[[2]]})
  output$cr_remove <- renderPrint({
    cr_rm_env()
    reduce_colin()[[3]]})

  # OCCURENCE FILTERING --------
  observe({
    req(env_base())
    tryCatch({
      if (terra::nlyr(env_base()) <= 2) {
        updateSelectInput(inputId = "coli_method", session = session,
                          choices = c("Pearson correlation" = "pearson", "Variance inflation factor" = "vif",
                                      "Principal component analysis" = "pca"))
      } else {
        updateSelectInput(inputId = "coli_method", session = session,
                          choices = c("Pearson correlation" = "pearson", "Variance inflation factor" = "vif",
                                      "Principal component analysis" = "pca", "Factorial analysis" = "fa"))
      }
    }, error = function(e){
      showModal(error_modal())
      output$error_modal <- renderText(paste(e))
    })
  })
  source(paste0(src_root, "occ_filtering.R"), verbose = FALSE, local = TRUE)$value

  # DATA PARTITION -------
  ## update -- sband partion
  observeEvent(input$sband_part_min, {
    updateNumericInput(session, "sband_part_max", min = input$sband_part_min + 1)
  })
  observeEvent(input$sband_part_number, {
    req(occ_used())
    updateNumericInput(session, "sband_part_min_occ", max = nrow(occ_used()) %/% input$sband_part_number)
  })
  ### update -- sblock partion
  observeEvent(input$min_res_mult, {
    updateNumericInput(session, "max_res_mult", min = input$min_res_mult + 1)
  })
  observeEvent(input$sblock_part_number, {
    req(occ_used())
    updateNumericInput(session, "sblock_min_occ", max = nrow(occ_used()) %/% input$sblock_part_number)
  })
  ### update -- senv partition
  observeEvent(input$min_n_groups, {
    updateNumericInput(session, "max_n_groups", min = input$min_n_groups + 1)
  })

  ## data partition
  ### partition function and arguments
  partition_call <- function(data, data_code) {
    d <- nm_cv(data, data_code)
    env <- nm_cv(env_layers(), "env")
    xy <- list(x = input$long_var, y = input$lat_var, pr_ab = "pr_ab")
    switch(input$partition_type,
           "part_random" = list(fun = "part_random", args = list(
             data = d, pr_ab = "pr_ab",
             method = switch(input$part_random_method,
                             "kfold" = c(method = "kfold", folds = input$kfold_number),
                             "rep_kfold" = c(method = "rep_kfold", folds = input$kfold_number,
                                             replicates = input$rep_kfold),
                             "loocv" = c(method = "loocv"),
                             "boot" = c(method = "boot", replicates = input$rep_boot,
                                        proportion = input$prop_boot)))),
           "part_sband" = list(fun = "part_sband", args = c(
             list(env_layer = env, data = d), xy,
             list(type = input$sband_lon_lat, n_part = input$sband_part_number,
                  min_bands = input$sband_part_min, max_bands = input$sband_part_max,
                  min_occ = input$sband_part_min_occ, prop = input$sband_part_prop))),
           "part_sblock" = list(fun = "part_sblock", args = c(
             list(env_layer = env, data = d), xy,
             list(n_part = input$sblock_part_number, min_res_mult = input$min_res_mult,
                  max_res_mult = input$max_res_mult, num_grids = input$num_grids,
                  min_occ = input$sblock_min_occ, prop = input$sblock_prop))),
           "part_senv" = list(fun = "part_senv", args = c(
             list(env_layer = env, data = d), xy,
             list(min_n_groups = input$min_n_groups, max_n_groups = input$max_n_groups,
                  min_occ = input$senv_min_occ, prop = input$senv_prop)))
    )
  }
  run_partition <- function(key, section, title, assign, data, data_code, pre = NULL, post = NULL) {
    p <- partition_call(data, data_code)
    set.seed(global_seed())
    run_recorded(key, section, title, assign, paste0("flexsdm::", p$fun),
                 getExportedValue("flexsdm", p$fun), p$args,
                 pre = c(pre, "set.seed(seed)"), post = post)
  }
  spatial_partition <- function() input$partition_type %in% c("part_sband", "part_sblock", "part_senv")

  data_partition <- eventReactive(input$divvy_data, {
    grid_env <- NULL
    repl <- input$partition_type == "part_random" && input$part_random_method %in% c("rep_kfold", "boot")
    post <- if (spatial_partition()) "part_groups <- part_preview$part$.part" else
      if (repl) "part_groups <- part_preview$.part1" else "part_groups <- part_preview$.part"
    if (input$partition_type %in% c("part_sband", "part_sblock")) {
      post <- c(post, "grid_env <- flexsdm::get_block(env_layer = env, best_grid = part_preview$grid)")
    }
    res <- run_partition("part_preview", "partition_preview",
                         "Sizes the background / pseudo-absence samples per partition",
                         "part_preview", occ_used(), "occ", post = post)

    if(input$partition_type == "part_random" && input$part_random_method %in% c("kfold", "loocv")){
      partion <- res
      prt <- if (input$part_random_method == "kfold") partion %>% dplyr::select(.part) else partion$.part
      parts <- partion$.part %>% table()
      cat("Partition group:\n")

    } else if (repl){
      partion <- res
      n_rep <- if (input$part_random_method == "rep_kfold") input$rep_kfold else input$rep_boot
      replic_fold_list = list()# list to hold partitions
      replic_parts <- partion[(length(partion) - n_rep + 1):length(partion)]
      for (i in colnames(replic_parts)) {
        replic_fold_list[[i]] <- replic_parts[, i] %>% table()
      }
      cat("Partition group:\n")
      prt <- partion %>% dplyr::select(.part1)
      parts <- replic_fold_list

    } else {
      parts <- res
      partion <- parts$part
      prt <- parts$part$.part
      if (input$partition_type %in% c("part_sband", "part_sblock")) {
        grid <- parts$grid ; x_y <- parts$part[c("x", "y")] ; pr_ab <- parts$part$pr_ab
        grid_env <- get_block(env_layer = env_layers(), best_grid = grid)
        output$part_sband_plot <- renderPlot({
          terra::plot(grid_env, col = gray.colors(20))
          points(x_y,
                 col = rainbow(8)[prt],
                 pch = c(1, 15)[pr_ab + 1]
          )
        })
      }
      cat("Partition group:\n"); print(prt %>% table()); cat("\n"); cat("Information about the best partition:"); cat("\n")
      parts <- data.frame(parts$best_part_info)
    }
    return(list(parts, grid_env, prt, partion))
  })

  output$part_out <- renderPrint({ data_partition()[[1]] })

  ## GENERATE BACKGROUND OR PSEUDO-ABSCENCE DATA ------------
  bpas_modal <- function(){
    modalDialog(title = h3("Generate background or psdeudo-absence points"), size = "l",
                footer = tagList(
                  modalButton("Ok"),
                  actionButton("generate_bpas", "Generate", style = bttn_primary_style)
                  ),
                fluidRow(column(8,
                                tabsetPanel(
                                  tabPanel("New distribution", shinycssloaders::withSpinner(plotOutput("pbas_plot"),
                                                                                            color = loader_color, type = loader_type)),
                                  tabPanel("New points", shinycssloaders::withSpinner(DT::dataTableOutput("new_points"),
                                                                                      color = loader_color, type = loader_type)),
                                  tabPanel("Updated occurrence", shinycssloaders::withSpinner(DT::dataTableOutput("complete_occ_data"),
                                                                                             color = loader_color, type = loader_type))
                                )),
                         column(4,
                                selectInput("bpas_type", label = "", choices = c("Background", "Pseudo-absence")),
                                conditionalPanel("input.bpas_type == 'Background'",
                                                 selectInput("bg_method", "Method", choices = bg_metho),
                                                 conditionalPanel("input.bg_method == 'thickening'",
                                                                  numericInput("thickening_width", "Width (m)", value = 1e5, min = 1)
                                                                  ),
                                                 numericInput("n_bg", "Multiply by", value = 10, min = 1, step = 1)),
                                conditionalPanel("input.bpas_type == 'Pseudo-absence'",
                                                 selectInput("pseudo_abs_method", "Method", choices = pseudo_abs_method),
                                                 conditionalPanel("input.pseudo_abs_method != 'random'",
                                                                  numericInput("pseudo_abs_width", "Width (m)", value = 1e5, min = 1))
                                                 )
                                )
                         )
                )
  }

  observeEvent(input$back_ps_ab_samp , { showModal(bpas_modal()) })
  ########----
  sd_occ_data <- eventReactive(input$generate_bpas, {
    req(occ_used())
    prt <- data_partition()[[3]] %>% table() %>% as.data.frame() %>% dplyr::select(2)
    if(input$partition_type == "part_random" && input$part_random_method %in% c("kfold", "rep_kfold")){
      n <- input$kfold_number
    } else if (input$partition_type == "part_random" && input$part_random_method == "loocv"){
      n <- nrow(occ_used())
    } else if(input$partition_type == "part_random" && input$part_random_method == "boot"){
      n <- 2
    } else if (input$partition_type == "part_sband"){
      n <- input$sband_part_number
    } else if(input$partition_type == "part_sblock"){
      n <- input$sblock_part_number
    } else if(input$partition_type == "part_senv"){
      n <- nrow(prt)
    }
    lit <- nm_lit
    nm_name <- nimo:::nm_name
    lon <- input$long_var; lat <- input$lat_var; sp <- input$species_var
    banded <- input$partition_type %in% c("part_sband", "part_sblock")
    w <- as.character(input$pseudo_abs_width)
    common <- paste0("    data = occ,\n    x = ", lit(lon), ",\n    y = ", lit(lat), ",\n")
    mask <- if (banded) "    rlayer = grid_env,\n    maskval = x,\n" else "    rlayer = env,\n    maskval = NULL,\n"
    head <- c("# Number of records per partition group",
              "n_points <- as.data.frame(table(part_groups))$Freq",
              "set.seed(seed)")

    #### background sample
    if (input$bpas_type == "Background" && banded) {
      method <- switch(input$bg_method, "random" = "random",
                       "thickening" = c(input$bg_method, input$thickening_width))
      code <- c(head, paste0(
        "bg <- lapply(1:", n, ", function(x) {\n",
        "  flexsdm::sample_background(\n", common,
        "    n = n_points[x] * ", lit(input$n_bg), ",\n",
        "    method = ", lit(method), ",\n", mask,
        "    calibarea = calib_area\n  )\n",
        "}) %>%\n  dplyr::bind_rows() %>%\n  dplyr::mutate(pr_ab = 0)"))
    } else if (input$bpas_type == "Background") {
      code <- c(head, paste0(
        "bg <- do.call(rbind, lapply(1:", n, ", function(x) {\n",
        "  bgpoint <- terra::spatSample(x = calib_area, method = \"random\", size = n_points[x] * ",
        lit(input$n_bg), ")\n",
        "  sf::st_coordinates(sf::st_as_sf(bgpoint)) %>%\n",
        "    as.data.frame() %>%\n",
        "    dplyr::rename(", nm_name(lon), " = X, ", nm_name(lat), " = Y) %>%\n",
        "    dplyr::mutate(pr_ab = 0, ", nm_name(sp), " = unique(occ[[", lit(sp), "]])) %>%\n",
        "    dplyr::relocate(dplyr::all_of(", lit(sp), "), .before = dplyr::all_of(", lit(lon), "))\n",
        "}))"))
    } else {
      ## pseudo-absence sample
      method <- switch(input$pseudo_abs_method,
                       "random" = "\"random\"",
                       "env_const" = "c(\"env_const\", env = env)",
                       "geo_const" = paste0("c(\"geo_const\", width = ", lit(w), ")"),
                       "geo_env_const" = paste0("c(\"geo_env_const\", width = ", lit(w), ", env = env)"),
                       "geo_env_km_const" = paste0("c(\"geo_env_km_const\", width = ", lit(w), ", env = env)"))
      code <- c(head, paste0(
        "bg <- lapply(1:", n, ", function(x) {\n",
        "  flexsdm::sample_pseudoabs(\n", common,
        "    n = n_points[x],\n",
        "    method = ", method, ",\n", mask,
        "    calibarea = calib_area\n  )\n",
        "}) %>%\n  dplyr::bind_rows() %>%\n  dplyr::mutate(pr_ab = 0)"))
    }
    title <- if (input$bpas_type == "Background") input$bg_method else input$pseudo_abs_method
    env <- run_code("bg", "background", paste0(input$bpas_type, " (", title, ")"),
                    paste(code, collapse = "\n"),
                    list(occ = occ_used(), part_groups = data_partition()[[3]],
                         grid_env = data_partition()[[2]], calib_area = calib_area_nimo(),
                         env = env_layers(), seed = global_seed()))
    return(env$bg)
  })

  c_occ_data <- reactive({
    dplyr::bind_rows(occ_used(), sd_occ_data())})
    output$new_points <- st_render_dt({sd_occ_data()})
    output$complete_occ_data <- st_render_dt({c_occ_data()})
    output$pbas_plot <- renderPlot({
    req(c_occ_data())
    ggplot()+
      geom_sf(data = shp_layer())+
      geom_sf(data = st_as_sf(calib_area_nimo()))+
      geom_point(data = c_occ_data(),
                 aes(x = !!sym(input$long_var), y = !!sym(input$lat_var), color = as.factor(!!sym("pr_ab"))))+
      labs(color = "Presence & absence", x = "Longitude", y = "Latitude")
  })

  ## VALIDED DATA PARTITION
  valided_dp <- eventReactive(input$valided_dp, {
    if(input$occ_type == "Presence"){
      req(c_occ_data())
      data <- c_occ_data()
      data_code <- "occ_pa"
      pre <- c("# Presences + background / pseudo-absence points",
               "occ_pa <- dplyr::bind_rows(occ, bg)")
    }else{
      data <- occ_used()
      data_code <- "occ"
      pre <- NULL
    }
    if (spatial_partition()) {
      run_partition("partition", "partition", "", "part_res", data, data_code,
                    pre = pre, post = "occ_part <- part_res$part")$part
    } else {
      run_partition("partition", "partition", "", "occ_part", data, data_code, pre = pre)
    }
  })
observeEvent(input$valided_dp, {
  tryCatch({
    if (is.data.frame(valided_dp())) {
      showNotification(
        id = "valided_dp",
        ui = "Partition validated",
        duration = 6,
        closeButton = TRUE,
        type = "message"
      )
    }
  })
})
  ## ETRACTION ---------
  ## update predictor selection
kept_colin_var <- reactiveValues(rc = NULL)

  observe({
    req(env_layers())
    reduce_colin <- names(env_layers())
    updateSelectInput(inputId = "extract_variables", choices = names(env_layers()),
                      selected = reduce_colin)

    tryCatch({kept_colin_var$rc <- reduce_colin()[[3]]}, error = function(e)print(e))
    if (!is.null(isolate(kept_colin_var$rc))) {
      reduce_colin <- names(env_layers())[!names(env_layers()) %in% reduce_colin()[[3]]]
      updateSelectInput(inputId = "extract_variables", choices = names(env_layers()),
                        selected = reduce_colin)
    }


  })
  ## process extraction
  extracted_df <- eventReactive(input$extract_data, {
    req(valided_dp())
    out <- run_recorded(
      "sdm_data", "extraction", "", "sdm_data", "nimo::nm_extract", nm_extract,
      list(data = nm_cv(valided_dp(), "occ_part"),
           longitude = ifelse(spatial_partition(), "x", input$long_var),
           latitude = ifelse(spatial_partition(), "y", input$lat_var),
           env_layer = nm_cv(env_layers(), "env"),
           current_crs = input$occ_current_crs,
           variables = input$extract_variables,
           filter_na = TRUE)
    )
    out
  })
  observeEvent(input$extract_data, model_data_source("extract"))
  output$extracted_data <- st_render_dt({extracted_df()}, width = "50%")
  ### Download extracted data
  shinyFiles::shinyFileSave(input, "save_extracted_data", roots = root, session = session)
  save_extracted_data_path <- reactive({
    req(input$save_extracted_data)
    shinyFiles::parseSavePath(roots = root, selection = input$save_extracted_data)
  })
  observe({
    req(save_extracted_data_path())
    tryCatch({
      if (nrow(save_extracted_data_path()) > 0 ) {
        write.csv(x = extracted_df(), file = as.character(save_extracted_data_path()$datapath))
      }
    }, error = error)
  })

  ## MODELING -----------
  ### import existing data
  observe({
    if (input$esm == TRUE) {
      updateCheckboxInput(inputId = "tuning", value = FALSE)
      updateCheckboxInput(inputId = "esm", value = TRUE)
    }
    if (input$tuning == TRUE) {
      updateCheckboxInput(inputId = "esm", value = FALSE)
      updateCheckboxInput(inputId = "tuning", value = TRUE)
    }
  })

  shinyFileChoose(input, id = "import_exiting_df4mod", roots = root,
                  filetype = c("txt", "csv"))
  exiting_df4mod_path <- reactive({
    unname(parseFilePaths(roots = root, selection = input$import_exiting_df4mod)$datapath)
  })

  exiting_df4mod <- reactive({
    req(length(exiting_df4mod_path()) > 0)
    tryCatch({
      run_recorded("sdm_data", "extraction", "Modelling data imported from file", "sdm_data",
                   "read.csv", read.csv, list(file = exiting_df4mod_path(), header = TRUE))
    }, error = error)
  })
  ## Data used for modelling: last loaded or extracted
  model_data_source <- reactiveVal(NULL)
  observeEvent(input$import_exiting_df4mod, {
    if (length(exiting_df4mod_path()) > 0) model_data_source("file")
  })

  ## update algorithm to use selection
  observeEvent(input$algorithm, {
    disable('fit_model_algorithm')
    updateSelectInput(inputId = "fit_model_algorithm",
                      choices = algorithm[names(algorithm[algorithm %in% input$algorithm])],
                      selected = algorithm[names(algorithm[algorithm %in% input$algorithm])]
                      )
  })
  ## data frame to use for modeling
  ready_df_mod <- reactive({
    req(model_data_source())
    if (model_data_source() == "file") {
      data <- exiting_df4mod()
    } else {
      data <- extracted_df()
    }
    req(is.data.frame(data))
    return(data)
  })

  ## fitting
  ## modal to show model output
  models_modals <- function(tle = "", bioclim = FALSE)({
    modalDialog(
      title = h3(tle, style = "text-align:left;"),
      footer = tagList(modalButton("Ok"),
                       downloadButton(outputId = "download_models_output", style = bttn_primary_style)),
      size = "l",
      tabsetPanel(id = "models_output",
        tabPanel("Model",
                 div(
                   style = "overflow-y: scroll; max-height: 350px;",
                   shinycssloaders::withSpinner(
                     verbatimTextOutput("xx_model"),
                     type = loader_type, color = loader_color)
                   )
          ),
        tabPanel("Performance metric", value = "xx_pm" , DT::DTOutput("xx_performance_metric")),
        tabPanel("Predicted suitability", DT::DTOutput("xx_predicted_suitability")),
        if(input$tuning) tabPanel(title = "Hyper-parameters performance", DT::DTOutput("xx_hyper_performance")),
        if(bioclim) tabPanel(title = "Envelope",
                             fluidRow(column(4, selectInput("bioc_env_x", "X", choices = input$bioc_predictors,
                                                            selected = input$bioc_predictors[1])),
                                      column(4, selectInput("bioc_env_y", "Y", choices = input$bioc_predictors,
                                                            selected = input$bioc_predictors[min(2, length(input$bioc_predictors))]))),
                             plotOutput("bioc_envelope_plot", height = "380px"))
      )
    )
  })


## ENSEMBLING ----
observeEvent(input$ensemble,
             {updateSelectInput(session = session, inputId = "ens_method", choices = ensemble,
                     selected = input$ensemble)}
)

output$selected_mod_lenght <- renderText(
  paste("The number of models selected for ensemble:", length(sst()) ))


## Code for a list of fitted models
models_code <- function(display_names) {
  paste0("list(", paste0(nimo:::nm_name(display_names), " = ",
                         vapply(display_names, model_var, ""), collapse = ", "), ")")
}

ens_fitting <- eventReactive(input$fit_ens, {
  req(length(model_list$models) > 0, sst())
  tryCatch({
    selected <- models_names_df()[sst(), ]
    run_recorded(
      "ensemble", "ensemble", "", "ens", "flexsdm::fit_ensemble", fit_ensemble,
      list(models = nm_cv(model_list$models[selected], models_code(selected)),
           ens_method = input$ens_method,
           thr = thr_arg(input$ens_thr, input$ens_sens),
           thr_model = thr_arg(input$ens_thr_model, input$ens_sens_model),
           metric = input$ens_metric))
  }, error = error)
})

output$ens_performance <- render_dt({ ens_fitting()$performance })
## Save Ensemble table
observe({
  shinyjs::hide("export_ens_table")
  if (input$fit_ens > 0) {
    shinyjs::show("export_ens_table")
  }
})
output$export_ens_table <- downloadHandler(
  filename = function() {
    paste0("model_ensemble_table", ".csv")
  },
  content = function(file) {
    write.csv(ens_fitting()$performance, file)
  },
  contentType = "text/csv"
)
## Hide
# observe({
#   shinyjs::hide("xx_hyper_performance")
#   if (input$tuning == TRUE) {
#     shinyjs::show("xx_hyper_performance")
#   }else{
#     shinyjs::hide("xx_hyper_performance")
#   }
# })

## POST-MODELING
## PREDICTIONS
observeEvent(input$esm, {
  if (input$esm == TRUE) {
    shinyjs::hide("model_category")
    shinyjs::hide("st_fitted_model_list_dt")
    shinyjs::show("es_fitted_model_list_dt")
    }
  else {
    shinyjs::show("model_category")
    shinyjs::show("st_fitted_model_list_dt")
  }
  })
observeEvent(input$model_category, {
  if(input$model_category == "Ensemble"){
    shinyjs::hide("st_fitted_model_list_dt")
  } else if (input$model_category == "Standard models"){
    shinyjs:: show("st_fitted_model_list_dt")
    shinyjs::hide("es_fitted_model_list_dt")
  }
})

predict_models <- reactive(
  tryCatch({
    if(input$esm == TRUE){
      req(esm_selected())
      nm <- esm_models_names_df()[esm_selected(), ][1]
      nm_cv(base::as.list(es_model_list$models[[nm]]), model_var(nm))
    } else if(input$esm == FALSE && input$model_category == "Ensemble"){
      req(ens_fitting())
      nm_cv(ens_fitting(), "ens")
    } else if(input$esm == FALSE && input$model_category == "Standard models"){
      req(st_selected())
      selected <- names(model_list$models)[st_selected()]
      nm_cv(lapply(st_selected(), function(x){
        concat_model(model_list$models, x)
      }), paste0("list(", paste(vapply(selected, model_var, ""), collapse = ", "), ")"))
    }
  }, error = error)

)
## set layer files path
shinyFiles::shinyFileChoose(input, "predict_area", roots = root, filetypes = c("shp", "kml", "kmz"))
predict_area_path <- reactive({
  unname(shinyFiles::parseFilePaths(roots = root, selection = input$predict_area)$datapath)
})

predict_area <- reactive({
  req(length(predict_area_path()) > 0)
  terra::vect(sf::read_sf( predict_area_path() ) %>%
                st_transform(crs = terra::crs(env_layers())))
})
#
prediction <- eventReactive(input$predict, {
  req(env_layers(), predict_models())
  tryCatch({
    area <- if (length(predict_area_path()) > 0) {
      nm_cv(predict_area(),
            paste0("terra::vect(sf::st_transform(sf::read_sf(", nm_lit(predict_area_path()),
                   "), crs = terra::crs(env)))"))
    } else NULL
    run_recorded(
      "prediction", "prediction", "", "pred", "nimo::nm_predict", nm_predict,
      list(models = predict_models(),
           pred = nm_cv(env_layers(), "env"),
           thr = thr_arg(input$predict_thr, input$predict_sens),
           predict_area = area,
           clamp = as.logical(input$predict_clamp),
           pred_type = input$predict_pred_type)
    )
  }, error = error)
})
## Modal to show predict output
predict_modal <- function(){
  modalDialog(title = h3("Predictions", style = "text-align:left;"),
              footer = modalButton("Ok"), size = "l",
              fluidRow(column(9,
                              shinycssloaders::withSpinner(
                                plotOutput("predict_raster_plot"),
                                color = loader_color, type = loader_type)
                              ),
                       column(3,
                              selectInput("pred_rasters", "Prediction", choices = c()),
                                downloadButton("download_predict", "Download", icon = icon("download"))
                              ))
              )
}

observeEvent(input$predict, {showModal(predict_modal())})

## Script code of each prediction layer
pred_layer_code <- reactiveVal(character(0))

pred_rst <- eventReactive(input$predict, {
  tryCatch({
    out <- predict_rasters(prediction = prediction())
    codes <- attr(out, "code")

    # Bioclim diagnostics
    if (!isTRUE(input$esm) && input$model_category == "Standard models") {
      selected <- names(model_list$models)[st_selected()]
      is_bioc <- vapply(model_list$models[selected],
                        function(x) inherits(x$model, "nm_bioclim"), logical(1))
      for (nm in selected[is_bioc]) {
        v <- model_var(nm)
        args <- list(model = nm_cv(model_list$models[[nm]], v), env_layer = nm_cv(env_layers(), "env"))
        out[[paste(nm, "- limiting factor")]] <- run_recorded(
          paste0("limiting:", v), "prediction", "Bioclim: variable limiting suitability",
          paste0(v, "_limiting"), "nimo::nm_bioclim_limiting", nm_bioclim_limiting, args)
        out[[paste(nm, "- envelope class")]] <- run_recorded(
          paste0("envelope:", v), "prediction", "Bioclim: core / marginal / outside envelope",
          paste0(v, "_envelope"), "nimo::nm_bioclim_envelope", nm_bioclim_envelope, args)
        codes[paste(nm, c("- limiting factor", "- envelope class"))] <-
          paste0(v, c("_limiting", "_envelope"))
      }
    }
    pred_layer_code(codes)
    out
  }, error = error)
})

observeEvent(input$predict, {
  req(pred_rst())
  updateSelectInput(inputId = "pred_rasters", choices = names(pred_rst()),
                    selected = names(pred_rst())[1])
  ### add from posteriori - overprediction correction
  updateSelectInput(inputId = "ov_p_cont_suit", choices = names(pred_rst()),
                    selected = names(pred_rst())[1])
})

observeEvent(input$pred_rasters, {
  req(input$pred_rasters)
  tryCatch({
    output$predict_raster_plot <- renderPlot({
      terra::plot(pred_rst()[[input$pred_rasters]],
                  main = names(pred_rst()[[input$pred_rasters]]))
    })
  }, error = error)
})

 ## MERGE PERFORMANCE
perf_models <- reactive(
  if(input$esm == TRUE){
    req(esm_selected())
    base::as.list(es_model_list$models[[esm_models_names_df()[esm_selected(), ][1]]])
  } else if(input$esm == FALSE && input$model_category == "Ensemble"){
    req(ens_fitting())
    list(ens_fitting())
  } else if(input$esm == FALSE && input$model_category == "Standard models"){
    req(st_selected())
    lapply(st_selected(), function(x){
      concat_model(model_list$models, x)
    })
  }

)
mperf <- reactive({ sdm_summarize(models = perf_models()) })

observeEvent(input$merge_model_perf, {
  tryCatch({output$model_perf_merged <- render_dt(mperf())}, error = error)
})

## Save performace table
observe({
  hide("save_model_perf_merged")
  req(input$merge_model_perf)
  if (!is.null(input$merge_model_perf)) {
    shinyjs::show("save_model_perf_merged")
  }
})
shinyFiles::shinyFileSave(input, "save_model_perf_merged", roots = root, session = session)
save_perf_path <- reactive({
  req(input$save_model_perf_merged)
  shinyFiles::parseSavePath(roots = root, selection = input$save_model_perf_merged)
})
observe({
  req(save_perf_path())
  tryCatch({
    req(save_perf_path())
    if (nrow(save_perf_path()) > 0 ) {
      write.csv(x = mperf(), file = as.character(save_perf_path()$datapath))
    }
  }, error = error)
})

## Download model output
observe({
  req(input$models_output)
  shinyjs::hide("download_models_output")
  if (input$models_output == "xx_pm") {
    shinyjs::show("download_models_output")
  } else{
    shinyjs::hide("download_models_output")
  }}
)
output$download_models_output <- downloadHandler(
  filename = function() {
    paste0("performance_metric", ".csv")
  },
  content = function(file) {
    write.csv(performance_metric, file)
  }
  )

## Download model prediction
output$download_predict <- downloadHandler(
  filename = function() {
    paste0(input$pred_rasters, ".tiff")
  },
  content = function(file) {
    terra::writeRaster(pred_rst()[[input$pred_rasters]], file, overwrite = T)
  }
)

## MODEL EXTRAPOLATION ----

## Predictors used for modelling
kept_env <- function() {
  env <- env_layers()
  removed <- tryCatch(reduce_colin()[[3]], error = function(e) NULL)
  keep <- intersect(names(env), names(ready_df_mod()))
  if (input$reduce_collin > 0 && is.character(removed)) keep <- setdiff(keep, removed)
  if (identical(keep, names(env))) return(nm_cv(env, "env"))
  nm_cv(env[[keep]], paste0("env[[", nm_lit(keep), "]]"))
}

model_extrapo <- eventReactive(input$extrapo_model, {
  req(env_layers(), ready_df_mod())
  tryCatch({
    args <- list(training_data = nm_cv(ready_df_mod(), "sdm_data"),
                 projection_data = kept_env(),
                 metric = input$extrap_metric,
                 n_cores = input$n_cores,
                 aggreg_factor = input$aggreg_factor,
                 pr_ab = "pr_ab")
    args <- args[names(args) %in% names(formals(flexsdm::extra_eval))]
    run_recorded("extrapolation", "extrapolation", "", "extra", "flexsdm::extra_eval",
                 flexsdm::extra_eval, args)
  }, error = error)
})
output$extrapo_raster <- renderPlot({
  req(model_extrapo())
  terra::plot(model_extrapo())})

# Download extrapolation raster
output$download_extrapo_raster <- downloadHandler(
  filename = function() {
    paste0(names(model_extrapo()), ".tiff")
  },
  content = function(file) {
    terra::writeRaster(model_extrapo(), file, overwrite = T)
  }
)



## POSTERIORI - CORRECTION
observe({
  req(ready_df_mod())
  updateSelectInput(inputId = "ov_p_lon", choices = colnames(ready_df_mod()))
  updateSelectInput(inputId = "ov_p_lat", choices = colnames(ready_df_mod()))
})
over_correct <- eventReactive(input$ov_p_correct, {
  req(pred_rst(), ready_df_mod())
  cont_suit <- pred_rst()[[input$ov_p_cont_suit]]
  tryCatch({
    run_recorded(
      "posteriori", "posteriori", input$ov_p_cont_suit, "pred_corrected",
      "flexsdm::msdm_posteriori", msdm_posteriori,
      list(records = nm_cv(ready_df_mod(), "sdm_data"),
           x = input$ov_p_lon,
           y = input$ov_p_lat,
           pr_ab = "pr_ab",
           cont_suit = nm_cv(cont_suit, "cont_suit"),
           method = input$ov_p_method,
           thr = thr_arg(input$ov_p_thr, input$predict_sens),
           buffer = if(input$ov_p_method == "bmcp"){input$ov_p_buffer} else {NULL},
           crs = nm_cv(terra::crs(cont_suit), "terra::crs(cont_suit)")),
      pre = paste0("cont_suit <- ", pred_layer_code()[[input$ov_p_cont_suit]],
                   " # ", input$ov_p_cont_suit)
    )
  }, error = error)
})

output$ov_p_raster <- renderPlot({
  req(over_correct())
  terra::plot(over_correct())
})

## Download overprediction correction
observeEvent(input$id_download_ov_p_correct, {
  req(over_correct())
  showModal(
    modalDialog(
      size = "m",
      selectInput(inputId = "correction_chosen", "Choose a correction",
                  choices = names(over_correct()),
                  selected = names(over_correct()[1])),
      footer = tagList(modalButton("Ok"), downloadButton("download_ov_p_correct"))
    )
  )
})


output$download_ov_p_correct <- downloadHandler(
  filename = function() {
    req(over_correct())
    prefix <- names(over_correct())[which(names(over_correct()) == input$correction_chosen)]
    paste0(prefix,"_overp_corrected", ".tiff")
  },
  content = function(file) {
    terra::writeRaster(over_correct()[input$correction_chosen], file, overwrite = T)
  }
)


## GBIF ACCESS
# Internet connection availability checking
having_ip <- function() {
  if (.Platform$OS.type == "windows") {
    ipmessage <- system("ipconfig", intern = TRUE)
  } else {
    ipmessage <- system("ifconfig", intern = TRUE)
  }
  validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
  any(grep(validIP, ipmessage))
}
# Get the internet status
internet_status <- reactive({
  if (!having_ip() && input$sidebar_menu == "gbif_access") {
    showNotification(
      id = "connection_status",
      ui = "No internet connection for GBIF data😴",
      duration = Inf,
      closeButton = TRUE,
      type = "error"
    )
  }
})
# Show an alert if the internet connection is no available
observe({
  internet_status()
})


# Update species suggestions as user types
observeEvent(input$search_by, {
  updateTextInput(inputId = "species_input",
                  placeholder = paste("Enter the", tolower(names(gbif_q[gbif_q == input$search_by]))))
  shinyWidgets::updatePickerInput(inputId = "species_suggestions", session = session,
                                  label = paste("Select the", tolower(names(gbif_q[gbif_q == input$search_by])))
                                  )
})

observeEvent(input$species_input, {
  req(input$species_input)
  tryCatch({
    search_term <- input$species_input
    search_by <- input$search_by
    species_suggested <- nm_gbif_suggestion(search_term, time_out = tmout()*60)
    shinyWidgets::updatePickerInput(inputId = "species_suggestions", session = session,
                      choices = species_suggested[, search_by], selected = input$species_suggestions)
  }, error = function(e){paste0(e)})
})

gbif_data <- eventReactive(input$load_gbif_data, {
  if (!is.null(drawn_poly())) {
    list(
      inside_drawn_ply()[[1]], # inside_drawn_ply() from ./inst/nimo/src/handle_sf.R
      inside_drawn_ply()[[2]]
    )}
  else if (input$country_filter != "" & input$use_geom_gbif == FALSE) {
    query_occ(query_params = query_params())}
  else if (input$use_geom_gbif == TRUE) {
    list(
      loc_geom()[[1]],
      loc_geom()[[2]]
    )
  }
})

## Hide country if geometry is defined
observe({
  if (input$use_geom_gbif == TRUE) {
    shinyjs::hide("country_filter")
  } else {
    shinyjs::show("country_filter")
  }
})
observe({
  if (input$use_geom_gbif == TRUE) {
    shinyjs::hide("country_filter")
  } else {
    shinyjs::show("country_filter")
  }
})

### Modal to show data queried
observeEvent(input$acces_gbif_data, {
  showModal(
    modalDialog(
      footer = NULL,
      title = tagList(
        actionButton("load_gbif_data", "Load", icon = icon("refresh", lib = "glyphicon"), style = bttn_second_style),
        actionButton("start_modeling", "Start Modeling", icon = icon("glyphicon-play", "glyphicon"), style = bttn_second_style),
        actionButton("add_to_map", "Add to map", icon = icon("plus"), style = bttn_third_style),
        shiny::downloadButton("export_occ",  "Export",  icon = shiny::icon("save"), style = bttn_third_style),
        modalButton("Close", icon = icon("remove-circle", lib = "glyphicon"))),
      size = "l",
      tags$div(
        shinycssloaders::withSpinner(
          DTOutput("gbif_occ_data"),
          color = loader_color, type = loader_type
        )
      )
    )
  )
})

output$gbif_occ_data <- DT::renderDT({
  input$acces_gbif_data
  gbif_data()[[1]]
}, options = list(scrollX = TRUE, scrollY = TRUE, searching = FALSE, lengthMenu = c(5, 10, 50, 100, 300, 500)),
selection = "single", editable = TRUE)

output$occ_gbif_dataset<- DT::renderDT({
  input$acces_gbif_data
  gbif_data()[[1]]
}, options = list(scrollX = TRUE, scrollY = TRUE, searching = FALSE, lengthMenu = c(5, 10, 50, 100, 300, 500)),
selection = "single", editable = TRUE)

## Save occ data ---1
output$export_occ <- downloadHandler(
  filename = function() {
    gsub("\\s", "_", paste0(input$species_suggestions, "_occurrence", ".csv"))
  },
  content = function(file) {
    occ_to_export <- gbif_data()[[1]] %>%
      dplyr::mutate(obs = paste0(decimalLatitude, decimalLongitude)) %>%
      dplyr::distinct(.data = ., obs, .keep_all = T) %>%
      dplyr::select(-obs)
    write.csv(occ_to_export, file)
  },
  contentType = "text/csv"
)

## Save occ data ---2
output$export_occ2 <- downloadHandler(
  filename = function() {
    gsub("\\s", "_", paste0(input$species_suggestions, "_occurrence", ".csv"))
  },
  content = function(file) {
    write.csv(gbif_data()[[1]], file)
  },
  contentType = "text/csv"
)

## leaflet
output$occ_map <- renderLeaflet({ llf() })
observeEvent(input$add_to_map, {lft_proxy()})
observe({ req(geom_vect()); lft_geom() })
observeEvent(input$clear_map, {
  output$occ_map <- renderLeaflet({ llf() })
})

# Get the drawn polygon
polyg <- reactiveValues(
  point = data.frame(lon = c(), lat = c())
)

observeEvent(input$occ_map_click,{
  req(input$occ_map_click)
  coord <- input$occ_map_click
  df <- data.frame(lon = coord[["lng"]], lat = coord[["lat"]])
  polyg$point <- rbind(polyg$point, df)
  if (nrow(polyg$point) > 2) {
    shinyjs::show("clear_map")
  }
})
# Hide country if polygon is drawn
observe({
  if (nrow(polyg$point) > 2) {
    shinyjs::hide("country_filter")
  } else {
    shinyjs::show("country_filter")
  }
})
observeEvent(input$clear_map, {polyg$point <- data.frame(lon = c(), lat = c())})

## clear button
observe({
  shinyjs::hide("clear_map")
  if (input$clear_map) {
    shinyjs::hide("clear_map")
    updateCheckboxInput(session, "use_geom_gbif", value = FALSE)
  }
})

observe({
  req(input$add_to_map)
  if (input$add_to_map) {
    shinyjs::show("clear_map")
  }
})
observe({
  req(geom_gbif())
  if (nrow(geom_gbif())) {
    shinyjs::show("clear_map")
  }
})

# citation - GBIF data
###
copy_button_update <- function (session, id = "copybtn", label = "Copy to Clipboard",
                                icon = NULL, text = "Sample Text", modal = FALSE){
  if (!is.null(icon))
    icon <- as.character(icon)
  session$sendCustomMessage("copybtnUpdate", list(id = id,
                                                  label = label, icon = icon, `data-clipboard-text` = text,
                                                  onclick = if (!modal) {
                                                    paste0("new ClipboardJS(\".btn\", document.getElementById(\"",
                                                           id, "\"));")
                                                  } else {
                                                    paste0("new ClipboardJS(\".btn\", { container: document.getElementById(\"",
                                                           id, "\") });")
                                                  }))
}
output$occ_citation <- renderPrint({
  gbif_data()[[2]]
})

observe({
  req(input$copy_citation_btn)
  copy_button_update(session,
                   id = "copy_citation_btn",
                   label = "Copy",
                   icon = icon("copy"),
                   text = paste(gbif_data()[[2]], collapse = "\n")
  )
})

shinyFileSave(input = input, "save_citation", root = root)
citation_file_path <- reactive({
  req(input$save_citation)
  shinyFiles::parseSavePath(roots = root, selection = input$save_citation)
})

observe({
  req(citation_file_path(), gbif_data()[[2]])
  if (nrow(citation_file_path()) > 0 ) {
    tryCatch(
      {
        writeLines(text = gbif_data()[[2]], as.character(citation_file_path()$datapath), sep = "\n\n")
      }, error = error
    )
  }
})


## Inject GBIF Data Into modeling Workflow
observeEvent(input$start_modeling, {
  tryCatch({
    if(is.data.frame(species_data())){
      showModal(data_customize_modal())}},
    error = function(e){
      showModal(error_modal())
      output$error_modal <- renderText(paste(e))
    })
})
# Open calibration menu when clicking 'valid data' from GBIF side
observeEvent(input$valid_data, {
  updateTabItems(session, "sidebar_menu", selected = "calibration")
})

# CONFIGURATION
config <- reactiveValues(seed = 123, timeout = 5)
global_seed <- function() config$seed
tmout <- function() config$timeout

observeEvent(input$save_config, {
  config$seed <- input$set_seed
  config$timeout <- input$sys_timeout
  showNotification(
    id = "save_config",
    ui = "Change saved",
    duration = 4,
    closeButton = FALSE,
    type = "message"
  )
})


## END SERVER


}

