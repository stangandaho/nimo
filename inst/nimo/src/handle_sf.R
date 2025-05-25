# Geometry handle
## Set vector files path
shinyFiles::shinyFileChoose(input, "location_filter", roots = root,
                            filetypes = c("shp", "kml"))
lf_path <- reactive({
  shinyFiles::parseFilePaths(roots = root, selection = input$location_filter)$datapath
})
## Process vector file reading
geom_vect <- reactive({
  req(lf_path())
  sf_use_s2(FALSE)
  suppressWarnings({geom_vect <- sf::st_simplify(sf::st_make_valid(sf::read_sf(lf_path())))})
  sf_use_s2(TRUE)
  if(is.na(sf::st_crs(geom_vect))){
    geom_vect <- sf::st_set_crs(geom_vect, 4326); geom_vect
  } else {
    geom_vect <- geom_vect %>%
      sf::st_transform(crs = st_crs(4326)); geom_vect
  }
  })

## Get geometry
geom_gbif <- reactive({
  geom_vect <- geom_vect()
  if (sf::st_geometry_type(geom_vect, by_geometry = F) %in% c("MULTIPOLYGON", "POLYGON")) {
    geom_vect <- geom_vect(); geom_vect
  } else {
    showModal(
      modalDialog(title = "", footer = modalButton("Ok"),
                  tags$h1("Choose a single polygon vector"))
    )
  }

})

## Local vector/shapefile
loc_geom <- reactive({
  query_params <- query_params()# query_params() from ./inst/nimo/src/query_gbif_occ_data.R
  if (nrow(geom_gbif()) > 1) {
    g <- sf::st_combine(geom_gbif()) %>% st_as_sf() %>% st_simplify()
    geom_wkt <- sf::st_as_text(sf::st_as_sfc(g))
  } else {
    geom_wkt <- sf::st_as_text(geom_gbif() %>%
                                 dplyr::select(geometry) %>%
                                 st_as_sfc() %>% st_simplify())
  }

  if (!is.null(query_params[["country"]])) {
    qp <- query_params[names(query_params) != "country"]
  } else {
    qp <- query_params
  }

  qp[["geometry"]] <- noquote(geom_wkt)
  all_occurrences_df <- query_occ(query_params = qp)
  all_occurrences_df
})

## Drawn polygon

drawn_poly <- eventReactive(input$acces_gbif_data, {
  if (!is.null(polyg$point$lon) & !is.null(polyg$point$lat)) {
    sf::st_as_sf(x = polyg$point, coords = c("lon", "lat"), crs = 4326)
  }
})

inside_drawn_ply <- reactive({
  query_params <- query_params()
  pl <- st_union(drawn_poly()) %>%
    sf::st_cast(x = ., to = "POLYGON")
    drawn_poly_wkt <- sf::st_as_text(pl, pretty = TRUE)
    if (!is.null(query_params[["country"]])) {
      qp <- query_params[names(query_params) != "country"]
    } else {
      qp <- query_params
    }
    qp[["geometry"]] <- noquote(drawn_poly_wkt)

    all_occurrences_df <- query_occ(query_params = qp)
    all_occurrences_df
})
