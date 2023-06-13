## geometry handle
## set vector files path
shinyFiles::shinyFileChoose(input, "location_filter", roots = root,
                            filetypes = c("shp", "kml", "kmz"))
lf_path <- reactive({
  shinyFiles::parseFilePaths(roots = root, selection = input$location_filter)$datapath
})
geom <- reactive({
  req(lf_path())
  sf::read_sf(lf_path())})
## Get geometry
geom_gbif <- function() {
  geom <- geom()

  if (sf::st_geometry_type(geom, by_geometry = F) != "MULTIPOLYGON" &&
      sf::st_geometry_type(geom, by_geometry = F) != "POLYGON") {
    cat("The geomtery must be a Polygon")
  }

  if(is.na(sf::st_crs(geom))){
    geom <- sf::st_set_crs(geom, 4326)
  } else {
    geom <- geom %>%
      sf::st_transform(crs = st_crs(4326))
  }
  return(geom)
}

## Loop through geometr(y)ies
mult_geom <- function(){
  geom_wkt <- sf::st_as_text(geom_gbif()$geometry)
  query_params <- query_params()

  if (!is.null(query_params[["country"]])) {
    query_params <- query_params[names(query_params) != "country"]
  }

    all_occurrences <- list()

    for (i in 1:length(geom_wkt)) {
      query_params[["geometry"]] <- geom_wkt[i]
      sing_geom_occ <- query_occ(query_params = query_params)
      all_occurrences <- append(all_occurrences, list(sing_geom_occ))
    }

    all_occurrences_df <- dplyr::bind_rows(all_occurrences)
    return(all_occurrences_df)
}

