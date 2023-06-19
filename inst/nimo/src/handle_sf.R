## geometry handle
## set vector files path
shinyFiles::shinyFileChoose(input, "location_filter", roots = root,
                            filetypes = c("shp", "kml"))
lf_path <- reactive({
  shinyFiles::parseFilePaths(roots = root, selection = input$location_filter)$datapath
})
geom_vect <- reactive({
  req(lf_path())
  geom_vect <- sf::st_make_valid(sf::read_sf(lf_path()))
  if(is.na(sf::st_crs(geom_vect))){
    geom_vect <- sf::st_set_crs(geom_vect, 4326)
  } else {
    geom_vect <- geom_vect %>%
      sf::st_transform(crs = st_crs(4326))
  }
  })

## Get geometry
geom_gbif <- function() {
  geom_vect <- geom_vect()
  if (sf::st_geometry_type(geom_vect, by_geometry = F) %in% c("MULTIPOLYGON", "POLYGON")) {
    return(geom_vect)
  } else {
    showModal(
      modalDialog(title = "", footer = modalButton("Ok"),
                  tags$h1("Choose a polygon vector"))
    )
  }

}

## Local vector
loc_geom <- function(){
  query_params <- query_params()
  if (nrow(geom_gbif()) > 1) {
    g <- sf::st_union(geom_gbif()) %>% sf::st_as_sf()
    geom_wkt <- sf::st_as_text(g$x)
  } else {
    geom_wkt <- sf::st_as_text(geom_gbif()$geometry)
  }
  if (!is.null(query_params[["country"]])) {
    qp <- query_params[names(query_params) != "country"]
  } else {
    qp <- query_params
  }

  qp[["geometry"]] <- geom_wkt
  all_occurrences_df <- query_occ(query_params = qp)
  return(all_occurrences_df)
}

inside_drawn_ply <- function(){
  query_params <- query_params()
  pl <- sf::st_cast(x = st_union(drawn_poly()), to = "POLYGON")
    drawn_poly_wkt <- sf::st_as_text(pl)
    if (!is.null(query_params[["country"]])) {
      qp <- query_params[names(query_params) != "country"]
    } else {
      qp <- query_params
    }

    qp[["geometry"]] <- drawn_poly_wkt
    print(qp)
    all_occurrences_df <- query_occ(query_params = qp)
    return(all_occurrences_df)
}
