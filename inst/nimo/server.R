## SERVER
server <- function(input, output, session) {
  src_root <- paste0(system.file("nimo", package = "nimo"), "/src/")#"./inst/nimo/src/"

  source(paste0(src_root, "global_variables.R"), verbose = FALSE, local = TRUE)$value
  source(paste0(src_root, "cust_functions.R"), verbose = FALSE, local = TRUE)$value
  source(paste0(src_root, "find_highly_coor_var.R"), verbose = FALSE, local = TRUE)$value
  source(paste0(src_root, "predictors_selection_update.R"), verbose = FALSE, local = TRUE)$value
  source(paste0(src_root, "render_model_fitting.R"), local = TRUE)[1]
  #source(paste0(src_root, "render_esm_model_fitting.R"), verbose = FALSE, local = TRUE)$value
  source(paste0(src_root, "model_fitting.R"), verbose = FALSE, local = TRUE)$value
  source(paste0(src_root, "model_tuning.R"), verbose = FALSE, local = TRUE)$value
  source(paste0(src_root, "esm_model_fitting.R"), verbose = FALSE, local = TRUE)$value
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
    shinyFiles::parseDirPath(roots = root, selection = input$browse_dir)
  })
  ## dir_setup invoking function
  project_dir <- eventReactive(input$dir_setup, {
    setwd(folder_path())# set working directory
      project_directory <- sdm_directory(
        main_dir = folder_path(),
        projections = "proj",
        calibration_area = if(input$cali_area != TRUE){NULL},
        algorithm = input$algorithm,
        ensemble = input$ensemble,
        threshold = TRUE)
      return(project_directory)
  })
  output$dir_setup_success <- renderText(paste("\n", {project_dir()}))

  # IMPORT DATA ---------
  ## set data file path
  shinyFiles::shinyFileChoose(input, "choose_data_file", roots = root,
                              filetypes = c("txt", "csv"))
  data_file_path <- reactive({
    shinyFiles::parseFilePaths(roots = root, selection = input$choose_data_file)$datapath
  })
  ## import data
  species_data <- reactive({
    if (is.null(input$start_modeling) && input$import_data_check == TRUE) {
    req(data_file_path())
    data <- read.csv(file = data_file_path(), header = TRUE) %>%
      dplyr::select(-1)
    } else if (input$start_modeling == 1 && input$import_data_check == TRUE) {
      req(data_file_path())
      data <- read.csv(file = data_file_path(), header = TRUE) %>%
        dplyr::select(-1)
    } else if (input$start_modeling == 1 && input$import_data_check == FALSE) {
      data <- gbif_data()[[1]]
    }
    data
  })

  ## set nodal to customize data importing
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
                                   column(4, selectInput("abscence", "Abscence", choices = c())))
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
    if (input$occ_type != "Presence") {
    df <- species_data() %>%
      dplyr::select(input$species_var, input$long_var, input$lat_var, input$occ_var, input$var_to_conserve) %>%
      dplyr::filter(!!sym(input$species_var) %in% input$unique_species &
                      !!sym(input$occ_var) %in% c(input$presence, input$abscence)) %>%
      dplyr::mutate(uni = paste0(!!sym(input$long_var), !!sym(input$lat_var))) #%>%
    w_df_uni <- df %>% dplyr::distinct(uni, .keep_all = TRUE) %>%
      dplyr::select(-uni) %>%
      dplyr::rename("pr_ab" = input$occ_var) %>%
      dplyr::filter(!is.na(pr_ab)) %>%
      dplyr::mutate(pr_ab = dplyr::case_when(pr_ab == input$presence ~ 1,
                                      pr_ab == input$abscence ~ 0))

      ## duplicate
    w_df_dupl <- df %>% dplyr::filter(duplicated(uni)) %>%
      dplyr::select(-uni)
    w_df_dupl <- w_df_dupl[complete.cases(w_df_dupl), ]# remove NA rows
    }
    else{
      df <- species_data() %>%
        dplyr::select(input$species_var, input$long_var, input$lat_var, input$var_to_conserve) %>%
        dplyr::filter(!!sym(input$species_var) == input$unique_species) %>%
        dplyr::mutate(uni = paste0(!!sym(input$long_var), !!sym(input$lat_var)))
      w_df_uni <- df %>% dplyr::distinct(uni, .keep_all = TRUE) %>%
        dplyr::select(-uni) %>%
        dplyr::mutate("pr_ab" = 1,
                      "{input$long_var}" := as.numeric(!!sym(input$long_var)),
                      "{input$lat_var}" := as.numeric(!!sym(input$lat_var)))
      ## duplicate
      w_df_dupl <- df %>% dplyr::filter(duplicated(uni)) %>%
        dplyr::select(-uni)
      w_df_dupl <- w_df_dupl[complete.cases(w_df_dupl), ]# remove NA rows

    }
    return(list(w_df_uni, w_df_dupl))
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
    shinyFiles::parseFilePaths(roots = root, selection = input$add_layer)$datapath
  })
  ## set shp data
  shp_layer <- reactive({
    req(layer_file_path())
    sf::read_sf(layer_file_path()) })

  occ_data <- reactive({sf::st_as_sf(x = wrangle_data()[[1]], coords = c(input$long_var, input$lat_var),
                                     crs = sf::st_crs(shp_layer())) %>% mutate(pr_ab = as.character(pr_ab))
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
occ_dt <- reactive({
  req(shp_layer())
  wd <- wrangle_data()[[1]]
  occ_dt <- sf::st_as_sf(wd, coords = c(input$long_var, input$lat_var), crs = crs(shp_layer())) %>%
    dplyr::bind_cols(wd[, c(input$long_var, input$lat_var)])
  occ_dt <- sf::st_intersection(x = occ_dt, y = shp_layer()) %>%
    as.data.frame() %>% dplyr::select(-geometry)
  occ_dt
})

  calib_area_nimo <- function(){
    occ_dt <- occ_dt()
    if (input$calib_area_method == "buffer") {
      cala <- calib_area(data = occ_dt, x = input$long_var,
                         y = input$lat_var, crs = terra::crs(shp_layer()),
                         method = c("buffer", width = as.numeric(input$calib_area_width)))
    } else if (input$calib_area_method == "bmcp"){
      cala <- calib_area(data = occ_dt, x = input$long_var,
                         y = input$lat_var, crs = terra::crs(shp_layer()),
                         method = c("bmcp", width = as.numeric(input$calib_area_width)))
    } else if (input$calib_area_method == "mcp"){
      cala <- calib_area(data = occ_dt, x = input$long_var,
                         y = input$lat_var, crs = terra::crs(shp_layer()),
                         method = c("mcp"))
    } else if (input$calib_area_method == "mask"){
      clusters <- terra::vect(shp_layer())
      cala <- calib_area(data = occ_dt, x = input$long_var,
                         y = input$lat_var, crs = terra::crs(shp_layer()),
                         method = c("mask", clusters, input$clusters_field))
    }
    return(cala)
  }

  ## Notify calibration area valid
  observeEvent(input$valid_calib_area, {
    req(calib_area_nimo())
    if (class(calib_area_nimo()) == "SpatVector") {
      showNotification(
        id = "cala_validity_status",
        ui = "Area calibrated and saved",
        duration = 4,
        closeButton = TRUE,
        type = "message"
      )
    }
  })
  ## plot calibrated area
  plot_cali_area <- eventReactive(input$valid_calib_area, {
      req(calib_area_nimo(), layer_file_path(), wrangle_data(), data_file_path(), shp_layer())
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
    shinyFiles::parseDirPath(roots = root, selection = input$pred_source)
  })

  pred_list <- reactive({
    list.files(path = predictors_path(), pattern = "[.tif!TIF!TIFF!tiff]$", full.names = T)
  })
  env_layers <- reactive({
    req(pred_list())
  tryCatch({
    ras <- terra::rast(pred_list())
    if (base::any(duplicated(names(ras)))) {
      names(ras) <- sub("\\.[^.]*$", "", basename(pred_list()))}
    ras
  }, error = function(e){
    showModal(error_modal())
    output$error_modal <- renderText(paste(e))
  })
  })
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
    modalDialog(title = h3("Pair plots of predictor", style = "text-align:left"), footer = modalButton("Ok"), size = "l",
                shinycssloaders::withSpinner(plotOutput("colin_corr"),
                                             color = loader_color, type = loader_type))
  }

  reduce_colin <- eventReactive(input$reduce_collin, {
    req(env_layers())
    if(input$coli_method == "pearson"){
      colin_var <- correct_colinvar(env_layer = env_layers(),
                                    method = c("pearson", th = as.character(input$pearson_threshold)))
      cr_df <- colin_var$cor_table # table
      enlayer <- colin_var$cor_variables # layer
      highly_corr_vars <- find_coor(cr_df, cutoff = input$pearson_threshold,
                                    names = T)#find_coor function from inst/nimo/src folder
      rm_enlayer <- highly_corr_vars
    } else if(input$coli_method == "vif"){
      colin_var <- correct_colinvar(env_layer = env_layers(), method = c("vif", th = as.character(input$vif_threshold)))
      cr_df <- colin_var$vif_table # table
      enlayer <- list()
      for (i in 1:terra::nlyr(colin_var$env_layer)) {
        l <- colin_var$env_layer[[i]]
        enlayer[[i]] <- l
      }
      rm_enlayer <- colin_var$removed_variables
    } else if(input$coli_method == "pca"){
      colin_var <- correct_colinvar(env_layer = env_layers(), method = c("pca"))
      cr_df <- colin_var$coefficients # table
      enlayer <- list()
      for (i in 1:terra::nlyr(colin_var$env_layer)) {
        l <- colin_var$env_layer[[i]]
        enlayer[[i]] <- l
      }
      rm_enlayer <- colin_var$cumulative_variance
    } else if(input$coli_method == "fa" & terra::nlyr(env_layers()) > 2){
        colin_var <- correct_colinvar(env_layer = env_layers(), method = c("fa"))
        cr_df <- colin_var$loadings # table
        enlayer <- colin_var$env_layer
        rm_enlayer <- colin_var$removed_variables
    }

      return(list(cr_df, enlayer, rm_enlayer, colin_var))
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
    req(env_layers())
    tryCatch({
      if (terra::nlyr(env_layers()) <= 2) {
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


  occ_filt_modal <- function(){
    modalDialog(title = h3("Occurrence data filtering", style = "text-align:left"), size = "l",
                footer = tagList(
                  modalButton("Ok"),
                  actionButton("valid_occ_filt", "Filter", icon = icon("check"), style = bttn_primary_style)
                  ),
                fluidRow(column(8,
                                tabsetPanel(
                                  tabPanel("Distribution",
                                           plotOutput("occ_filt_plot")),
                                  tabPanel("Filtered data",
                                           DT::dataTableOutput("occ_filtered_data", height = "500px"))
                )),
                         column(4,
                                selectInput("occ_filt_type", "Filtering type", choices = c("Geographical", "Environmental")),
                                conditionalPanel("input.occ_filt_type == 'Environmental'",
                                                 numericInput("occ_filt_nbins", "Number of bin", value = 1, min = 1, step = 1)),
                                conditionalPanel("input.occ_filt_type == 'Geographical'",
                                                 selectInput("enlayer_to_plot", label = NULL,  choices = c()),
                                                 selectInput("occ_filt_method", "Filtering method",
                                                             choices = occ_filt_method),
                                                 conditionalPanel("input.occ_filt_method == 'cellsize'",
                                                                  numericInput("occ_filt_cellsize", "Factor", min = 1, value = 2, step = 1)),
                                                 conditionalPanel("input.occ_filt_method == 'defined'",
                                                                  numericInput("occ_filt_defined", "Distance (km)", min = 0, value = 1))),
                                shinySaveButton(id = "save_occ_filt", label = "Save",  title = "Save occurrence data filtered",
                                                filename = "", filetype = list(CSV = "csv", `Plain text` = "txt"),
                                                icon = icon("save"), style = bttn_primary_style)
                                )),
                )
  }

  observeEvent(input$occ_filt, {
    showModal(occ_filt_modal())
    updateSelectInput(session, "enlayer_to_plot", choices = names(env_layers()))
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
      write.csv(x = occ_filt_out()[[2]], file = as.character(save_occ_filt_path()$datapath))
    }
    })

  occ_filt_out <- eventReactive(input$valid_occ_filt, {
    # data frame
    data <- wrangle_data()[[1]]#[c(input$long_var, input$lat_var)]
    if(input$occ_filt_method == "moran" && input$occ_filt_type == "Geographical"){
      occ_filtered <- occfilt_geo(data = data, x = input$long_var, y = input$lat_var, env_layer = env_layers(),
                                  method = "moran", prj = terra::crs(env_layers()))
    } else if (input$occ_filt_method == "cellsize" && input$occ_filt_type == "Geographical") {
      occ_filtered <- occfilt_geo(data = data, x = input$long_var, y = input$lat_var, env_layer = env_layers(),
                                  method = c("cellsize", factor = input$occ_filt_cellsize), prj = terra::crs(env_layers()))
    } else if (input$occ_filt_method == "defined" && input$occ_filt_type == "Geographical") {
      occ_filtered <- occfilt_geo(data = data, x = input$long_var, y = input$lat_var, env_layer = env_layers(),
                                  method = c("defined", d = input$occ_filt_defined), prj = terra::crs(env_layers()))
    } else if (input$occ_filt_type == "Environmental") {
      data$id <- 1:nrow(data)
      occ_filtered <- occfilt_env(data = data, x = input$long_var, y = input$lat_var, env_layer = env_layers(), id = "id",
                                  nbins = input$occ_filt_nbins)
    }
    # plot
    occ_env_layer_plot <- function(){
      terra::plot(env_layers()[input$enlayer_to_plot], main = input$enlayer_to_plot)
      points(occ_filtered[c(input$long_var, input$lat_var)])
    }
    return(list(occ_env_layer_plot(), occ_filtered))
  })
  output$occ_filt_plot <- renderPlot({ occ_filt_out()[[1]] })
  output$occ_filtered_data <- DT::renderDataTable({ occ_filt_out()[[2]] })



  # DATA PARTITION -------
  ## update -- sband partion
  observeEvent(input$sband_part_min, {
    updateNumericInput(session, "sband_part_max", min = input$sband_part_min + 1)
  })
  observeEvent(input$sband_part_number, {
    req(wrangle_data()[[1]])
    updateNumericInput(session, "sband_part_min_occ", max = nrow(wrangle_data()[[1]]) %/% input$sband_part_number)
  })
  ### update -- sblock partion
  observeEvent(input$min_res_mult, {
    updateNumericInput(session, "max_res_mult", min = input$min_res_mult + 1)
  })
  observeEvent(input$sblock_part_number, {
    req(wrangle_data()[[1]])
    updateNumericInput(session, "sblock_min_occ", max = nrow(wrangle_data()[[1]]) %/% input$sblock_part_number)
  })
  ### update -- senv partition
  observeEvent(input$min_n_groups, {
    updateNumericInput(session, "max_n_groups", min = input$min_n_groups + 1)
  })

  ## data partition
  data_partition <- eventReactive(input$divvy_data, {
    grid_env <- NULL
    set.seed(global_seed())
    if(input$partition_type == "part_random" && input$part_random_method == "kfold"){
      partion <- part_random(data = wrangle_data()[[1]], pr_ab = "pr_ab",
                             method = c(method = "kfold", folds = input$kfold_number))
      prt <- partion %>%  dplyr::select(.part)
      parts <- partion$.part %>% table()
      cat("Partition group:\n")

    } else if (input$partition_type == "part_random" && input$part_random_method == "rep_kfold"){
      partion <- part_random(data = wrangle_data()[[1]], pr_ab = "pr_ab",
                             method = c(method = "rep_kfold", folds = input$kfold_number, replicates = input$rep_kfold))
                  replic_fold_list = list()# list to hold partitions
                  replic_parts <- partion[(length(partion) - input$rep_kfold + 1):length(partion)]
                  for (i in colnames(replic_parts)) {
                    parts <- replic_parts[, i] %>% table()
                    replic_fold_list[[i]] <- parts
                  }
      cat("Partition group:\n")
      prt <- partion %>%  dplyr::select(.part1)
      parts <- replic_fold_list

    } else if (input$partition_type == "part_random" && input$part_random_method == "loocv"){
      partion <- part_random(data = wrangle_data()[[1]], pr_ab = "pr_ab",
                             method = c(method = "loocv"))
      cat("Partition group:\n")
      prt <- partion$.part
      parts <- partion$.part %>% table()

    } else if(input$partition_type == "part_random" && input$part_random_method == "boot"){
      partion <- part_random(data = wrangle_data()[[1]], pr_ab = "pr_ab",
                             method = c(method = "boot", replicates = input$rep_boot,
                                        proportion = input$prop_boot))
      cat("Partition group:\n")
                  replic_fold_list = list()# list to hold partitions
                  replic_parts <- partion[(length(partion) - input$rep_boot + 1):length(partion)]
                  for (i in colnames(replic_parts)) {
                    parts <- replic_parts[, i] %>% table()
                    replic_fold_list[[i]] <- parts
                  }
      parts <- replic_fold_list
      prt <- partion %>% select(.part1)

    } else if (input$partition_type == "part_sband"){
      print(env_layers())
      parts <- part_sband(env_layer = env_layers(), data = wrangle_data()[[1]],
        x = input$long_var, y = input$lat_var, pr_ab = "pr_ab",
        type = input$sband_lon_lat,
        n_part = input$sband_part_number,
        min_bands = input$sband_part_min,
        max_bands = input$sband_part_max,
        min_occ = input$sband_part_min_occ,
        prop = input$sband_part_prop
      )
      partion <- parts$part
      grid <- parts$grid ; x_y <- parts$part[c("x", "y")] ; pr_ab <- parts$part$pr_ab
      grid_env <- get_block(env_layer = env_layers(), best_grid = grid)
      prt <- parts$part$.part
      cat("Partition group:\n"); print(prt %>% table()); cat("\n"); cat("Information about the best partition:"); cat("\n")
      output$part_sband_plot <- renderPlot({
        terra::plot(grid, col = gray.colors(20))
        points(x_y,
               col = rainbow(8)[prt],
               pch = c(1, 15)[pr_ab + 1]
              )
        })
      parts <- data.frame(parts$best_part_info)

    } else if(input$partition_type == "part_sblock") {
      parts <- part_sblock(
        env_layer = env_layers(),
        data = wrangle_data()[[1]],
        x = input$long_var, y = input$lat_var, pr_ab = "pr_ab",
        n_part = input$sblock_part_number,
        min_res_mult = input$min_res_mult,
        max_res_mult = input$max_res_mult,
        num_grids = input$num_grids,
        min_occ = input$sblock_min_occ,
        prop = input$sblock_prop
      )
      partion <- parts$part
      grid <- parts$grid ; x_y <- parts$part[c("x", "y")] ; pr_ab <- parts$part$pr_ab
      grid_env <- get_block(env_layer = env_layers(), best_grid = grid)
      prt <- parts$part$.part
      cat("Partition group:\n"); print(prt %>% table()); cat("\n"); cat("Information about the best partition:"); cat("\n")

      output$part_sband_plot <- renderPlot({
        terra::plot(grid_env, col = gray.colors(20))
        points(x_y,
               col = rainbow(8)[prt],
               pch = c(1, 15)[pr_ab + 1]
        )
      })
      parts <- data.frame(parts$best_part_info)

    } else if(input$partition_type == "part_senv"){
      parts <- part_senv(
        env_layer = env_layers(),
        data = wrangle_data()[[1]],
        x = input$long_var, y = input$lat_var, pr_ab = "pr_ab",
        min_n_groups = input$min_n_groups,
        max_n_groups = input$max_n_groups,
        min_occ = input$senv_min_occ,
        prop = input$senv_prop
      )
      partion <- parts$part
      prt <- parts$part$.part
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
    req(wrangle_data()[[1]])
    prt <- data_partition()[[3]] %>% table() %>% as.data.frame() %>% dplyr::select(2)
    if(input$partition_type == "part_random" && !input$part_random_method %in% c("rep_kfold", "loocv", "boot")){
      n <- input$kfold_number
    } else if(input$partition_type == "part_random" && input$part_random_method == "rep_kfold"){
      n <- input$kfold_number
    } else if (input$partition_type == "part_random" && input$part_random_method == "loocv"){
      n <- nrow(wrangle_data()[[1]])
    } else if(input$partition_type == "part_random" && input$part_random_method == "boot"){
      n <- 2
      #part <-  data.frame(data_partition()[[3]])[, 2]
    } else if (input$partition_type == "part_sband"){
      n <- input$sband_part_number
      rlayer <- data_partition()[[2]]
    } else if(input$partition_type == "part_sblock"){
      n <- input$sblock_part_number
      rlayer <- data_partition()[[2]]
    } else if(input$partition_type == "part_senv"){
      n <- nrow(prt)
    }
    #### background sample
    if(input$bpas_type == "Background" && input$partition_type %in% c("part_sband", "part_sblock")){
      bg <- lapply(1:n, function(x) {
        flexsdm::sample_background(
          data = wrangle_data()[[1]],
          x = input$long_var, y = input$lat_var,
          n = prt[x, 1] * (input$n_bg),
          method = switch(input$bg_method,
                          "random" = "random",
                          "thickening" = c(input$bg_method, input$thickening_width)),
          rlayer = rlayer,
          maskval = x,
          calibarea = calib_area_nimo()
        )
      }) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(pr_ab = 0)

    }
    else if(input$bpas_type == "Background" && !input$partition_type %in% c("part_sband", "part_sblock")){
      bg_list <- lapply(1:n, function(x){
        bgpoint <- terra::spatSample(x = calib_area_nimo(), method = "random",
                                     size = prt[x, 1]* (input$n_bg))
        sf::st_coordinates(st_as_sf(bgpoint)) %>% as.data.frame() %>%
          dplyr::rename("{input$long_var}" := X, "{input$lat_var}" := Y) %>%
          dplyr::mutate(pr_ab = 0,
                 "{input$species_var}" := unique(dplyr::pull(wrangle_data()[[1]],
                                                             var = input$species_var))) %>%
          dplyr::relocate(!!sym(input$species_var), .before = !!sym(input$long_var))
      }) #%>% dplyr::bind_rows()
      bg <- do.call(rbind, bg_list)
    }

    ## pseudo-abscence sample
    else if(input$bpas_type == "Pseudo-absence" && input$partition_type %in% c("part_sband", "part_sblock")){
      w <- as.character(input$pseudo_abs_width)
      bg <- lapply(1:n, function(x) {
        sample_pseudoabs(
          data = wrangle_data()[[1]],
          x = input$long_var, y = input$lat_var,
          n = prt[x, 1] ,
          method = switch(input$pseudo_abs_method,
                          "random" = 'random',
                          "env_const" = c('env_const', env = env_layers()),
                          "geo_const" = c('geo_const', width = w),
                          "geo_env_const" = c('geo_env_const', width = w, env = env_layers()),
                          "geo_env_km_const" = c('geo_env_km_const', width = w, env = env_layers())
                          ),
          rlayer = rlayer,
          maskval = x,
          calibarea = calib_area_nimo()
        )
      }) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(pr_ab = 0)
    }else if(input$bpas_type == "Pseudo-absence" && !input$partition_type %in% c("part_sband", "part_sblock")){
      w <- as.character(input$pseudo_abs_width)
      bg <- lapply(1:n, function(x) {
        sample_pseudoabs(
          data = wrangle_data()[[1]],
          x = input$long_var, y = input$lat_var,
          n = prt[x, 1] ,
          method = switch(input$pseudo_abs_method,
                          "random" = 'random',
                          "env_const" = c('env_const', env = env_layers()),
                          "geo_const" = c('geo_const', width = w),
                          "geo_env_const" = c('geo_env_const', width = w, env = env_layers()),
                          "geo_env_km_const" = c('geo_env_km_const', width = w, env = env_layers())
          ),
          rlayer = env_layers(),
          maskval = NULL,
          calibarea = calib_area_nimo()
        )
      }) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(pr_ab = 0)
    }
    return(bg)
  })

  c_occ_data <- reactive({
    dplyr::bind_rows(wrangle_data()[[1]], sd_occ_data())})
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
    }else{
      data <- wrangle_data()[[1]]
    }

      if(input$partition_type == "part_random" && input$part_random_method == "kfold"){
      partion <- part_random(data = data, pr_ab = "pr_ab",
                             method = c(method = "kfold", folds = input$kfold_number))
      val_dp <- partion
    } else if (input$partition_type == "part_random" && input$part_random_method == "rep_kfold"){
      partion <- part_random(data = data, pr_ab = "pr_ab",
                             method = c(method = "rep_kfold", folds = input$kfold_number, replicates = input$rep_kfold))
      val_dp <- partion
    } else if (input$partition_type == "part_random" && input$part_random_method == "loocv"){
      partion <- part_random(data = data, pr_ab = "pr_ab",
                             method = c(method = "loocv"))
      val_dp <- partion
    } else if(input$partition_type == "part_random" && input$part_random_method == "boot"){
      partion <- part_random(data = data, pr_ab = "pr_ab",
                             method = c(method = "boot", replicates = input$rep_boot,
                                        proportion = input$prop_boot))
      val_dp <- partion
    } else if (input$partition_type == "part_sband"){
      partion <- part_sband(
        env_layer = env_layers(), data = data,
        x = input$long_var, y = input$lat_var, pr_ab = "pr_ab",
        type = input$sband_lon_lat, n_part = input$sband_part_number,
        min_bands = input$sband_part_min, max_bands = input$sband_part_max,
        min_occ = input$sband_part_min_occ, prop = input$sband_part_prop
      )
      val_dp <- partion$part
    } else if(input$partition_type == "part_sblock") {
      partion <- part_sblock(
        env_layer = env_layers(), data = data,
        x = input$long_var, y = input$lat_var, pr_ab = "pr_ab", n_part = input$sblock_part_number,
        min_res_mult = input$min_res_mult, max_res_mult = input$max_res_mult,
        num_grids = input$num_grids, min_occ = input$sblock_min_occ, prop = input$sblock_prop
      )
      val_dp <- partion$part
    } else if(input$partition_type == "part_senv"){
      partion <- part_senv(
        env_layer = env_layers(),
        data = data,
        x = input$long_var, y = input$lat_var, pr_ab = "pr_ab",
        min_n_groups = input$min_n_groups, max_n_groups = input$max_n_groups,
        min_occ = input$senv_min_occ, prop = input$senv_prop
      )
      val_dp <- partion$part
    }
    return(val_dp)
  })
observeEvent(input$valided_dp, {
  tryCatch({
    if (is.data.frame(valided_dp())) {
      showNotification(
        id = "valided_dp",
        ui = "Partition validated",
        duration = 4,
        closeButton = TRUE,
        type = "message"
      )
    }
  })
})
  ## ETRACTION ---------
  ## update predictor selection
  observe({
    req(env_layers())
    updateSelectInput(inputId = "extract_variables", choices = names(env_layers()),
                      selected = names(env_layers()) [ !names(env_layers()) %in% reduce_colin()[[3]] ]
                      )
  })
  ## process extraction
  extracted_df <- eventReactive(input$extract_data, {
    req(valided_dp())
    sdm_extract(
      data = valided_dp(),
      x = ifelse(input$partition_type %in% c("part_sband", "part_sblock", "part_senv"), "x", input$long_var),
      y = ifelse(input$partition_type %in% c("part_sband", "part_sblock", "part_senv"), "y", input$lat_var),
      env_layer = env_layers(),
      variables = input$extract_variables,
      filter_na = TRUE
    )
  })
  output$extracted_data <- st_render_dt({extracted_df()}, width = "50%")
  ### Download extracted data
  shinyFiles::shinyFileSave(input, "save_extracted_data", roots = root, session = session)
  save_extracted_data_path <- reactive({
    req(input$save_extracted_data)
    shinyFiles::parseSavePath(roots = root, selection = input$save_extracted_data)
  })
  observe({
    req(save_extracted_data_path())
    if (nrow(save_extracted_data_path()) > 0 ) {
      write.csv(x = extracted_df(), file = as.character(save_extracted_data_path()$datapath))
    }
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
    parseFilePaths(roots = root, selection = input$import_exiting_df4mod)$datapath
  })

  exiting_df4mod <- reactive({
    tryCatch({
      req(exiting_df4mod_path())
      read.csv(file = exiting_df4mod_path(), header = T)
    }, error = error)
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
    req(exiting_df4mod_path())
    if(exiting_df4mod_path() != ""){
      req(exiting_df4mod())
      data <- exiting_df4mod()
    } else {
      req(extracted_df())
      data <- extracted_df()
    }
    return(data)
  })

  ## fitting
  ## modal to show model output
  models_modals <- function(tle = "")({
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
        if(input$tuning) tabPanel(title = "Hyper-parameters performance", DT::DTOutput("xx_hyper_performance"))
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


ens_fitting <- eventReactive(input$fit_ens, {
  req(es_model_list$models, esm_models_names_df(), sst())
  tryCatch({
    fit_ensemble(
      models = model_list$models[ models_names_df()[sst(), ] ],
      ens_method = input$ens_method,
      thr = if(any(input$ens_thr %in% c("sensitivity"))) {
        c(input$ens_thr, "sens" = as.character(input$ens_sens))
      } else {input$ens_thr},
      thr_model = if(any(input$ens_thr_model %in% c("sensitivity"))) {
        c(input$ens_thr_model, "sens" = as.character(input$ens_sens_model))
      } else {input$ens_thr_model},
      metric = input$ens_metric)
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
      base::as.list(es_model_list$models[[esm_models_names_df()[esm_selected(), ]]])
    } else if(input$esm == FALSE && input$model_category == "Ensemble"){
      req(ens_fitting())
      ens_fitting()
    } else if(input$esm == FALSE && input$model_category == "Standard models"){
      req(st_selected())
      lapply(st_selected(), function(x){
        concat_model(model_list$models, x)
      })
    }
  }, error = error)

)
## set layer files path
shinyFiles::shinyFileChoose(input, "predict_area", roots = root, filetypes = c("shp", "kml", "kmz"))
predict_area_path <- reactive({
  shinyFiles::parseFilePaths(roots = root, selection = input$predict_area)$datapath
})

predict_area <- reactive({
  req(predict_area_path())
  terra::vect(sf::read_sf( predict_area_path() ) %>%
                st_transform(crs = terra::crs(env_layers())))
})
#
prediction <- eventReactive(input$predict, {
  req(env_layers())
  tryCatch({
    sdm_predict(
      models = predict_models(),
      pred = env_layers(),
      thr = if(any(input$predict_thr %in% c("sensitivity"))) {
        c(input$predict_thr, "sens" = as.character(input$predict_sens))
      } else{input$predict_thr},
      #predict_area = predict_area(),
      clamp = input$predict_clamp,
      pred_type = input$predict_pred_type,
    )
  }, eraror = error)
})
## Modal to show predict output
predict_modal <- function(){
  modalDialog(title = h3("Spatial predictions", style = "text-align:left;"),
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

pred_rst <- eventReactive(input$predict, {
  tryCatch({
    predict_rasters(prediction = prediction())
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
    req(es_selected())
    base::as.list(es_model_list$models[[esm_models_names_df()[es_selected(), ]]])
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

model_extrapo <- eventReactive(input$extrapo_model, {
  req(reduce_colin())
  tryCatch({
    if (input$reduce_collin > 0 && !any(reduce_colin()[[3]] %in% "")) {
      env_layers <- env_layers()[[!names(env_layers()) %in% reduce_colin()[[3]]]]
    } else {
      env_layers <- env_layers()
    }
    extra_eval(
      training_data = ready_df_mod(),
      projection_data = env_layers,
      metric = input$extrap_metric,
      n_cores = input$n_cores,
      aggreg_factor = input$aggreg_factor,
      pr_ab = "pr_ab"
    )
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
  if (input$reduce_collin > 0 && !any(reduce_colin()[[3]] %in% "")) {
    env_layers <- env_layers()[[!names(env_layers()) %in% reduce_colin()[[3]]]]
  } else {
    env_layers <- env_layers()
  }
  cont_suit <- pred_rst()[[input$ov_p_cont_suit]]
  tryCatch({
    msdm_posteriori(
      records = ready_df_mod(),
      x = input$ov_p_lon,
      y = input$ov_p_lat,
      pr_ab = "pr_ab",
      cont_suit = cont_suit,
      method = input$ov_p_method,
      thr = if(any(input$ov_p_thr %in% c("sensitivity"))) {
        c(input$ov_p_thr, "sens" = as.character(input$predict_sens))
      } else{input$ov_p_thr},
      buffer = if(input$ov_p_method == "bmcp"){input$ov_p_buffer} else {NULL},
      crs = terra::crs(cont_suit)
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

# Function to fetch species suggestions from GBIF API
fetch_species_suggestions <- function(search_term) {
  tryCatch({
    if (having_ip()) {
      url <- paste0("https://api.gbif.org/v1/species/suggest?q=", URLencode(search_term))
      response <- httr::GET(url, httr::timeout(tmout()*60))
      species_list <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
      if (!is.null(species_list)) {
        return(species_list)
      }
    }
  }, error = error)
}

# Update species suggestions as user types
observeEvent(input$search_by, {
  updateTextInput(inputId = "species_input",
                  placeholder = paste("Enter the", tolower(names(gbif_q[gbif_q == input$search_by]))))
  shinyWidgets::updatePickerInput(inputId = "species_suggestions", session = session,
                                  label = paste("Select the", tolower(names(gbif_q[gbif_q == input$search_by])))
                                  )
})

observeEvent(input$species_input, {
  search_term <- input$species_input
  search_by <- input$search_by
  species_suggested <- fetch_species_suggestions(search_term)
  tryCatch({
    shinyWidgets::updatePickerInput(inputId = "species_suggestions", session = session,
                      choices = species_suggested[, search_by], selected = input$species_suggestions)
  }, error = function(e){return(e)})
})

gbif_data <- eventReactive(input$load_gbif_data, {
  if (!is.null(drawn_poly())) {
    list(
      inside_drawn_ply()[[1]], # inside_drawn_ply() from ./inst/nimo/src/handle_sf.R
      inside_drawn_ply()[[2]]
    )}
  else if (input$country_filter != "" & input$use_geom_gbif == FALSE) {
    list(
      query_occ(query_params = query_params())[[1]],
      query_occ(query_params = query_params())[[2]]
    )}
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
global_seed <- reactive(123)
tmout <- reactive(5)

observeEvent(input$save_config, {
global_seed <<- reactive(input$set_seed)
tmout <<- reactive(input$sys_timeout)
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



