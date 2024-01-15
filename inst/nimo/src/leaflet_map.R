## Set leaflet map
llf <- function() {
  simple_lft <- leaflet::leaflet() %>%
    setView(lng = 1.8, lat = 11, zoom = 8)%>%
    addTiles(attribution = "Nimo") %>%
    addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "Natural") %>%
    addProviderTiles("OpenTopoMap", group = "OpenTopoMap") %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "Natural", "OpenTopoMap"),
      options = layersControlOptions(collapsed = T)
    ) %>%
    addMiniMap(position = "bottomright", height = 100, width = 100) %>%
    addDrawToolbar(targetGroup='draw', position = "topright",
                   markerOptions = FALSE, polylineOptions = FALSE,
                   editOptions = FALSE, circleMarkerOptions = FALSE,
                   circleOptions = FALSE, rectangleOptions = FALSE,
                   singleFeature = TRUE) %>%
    addMeasure(
      position = "bottomright",
      primaryLengthUnit = "meters",
      primaryAreaUnit = "sqmeters",
      activeColor = "#3D535D",
      completedColor = "#7D4479")
  return(simple_lft)
}

## Control the map that has already been rendered adding occurrence
lft_proxy <- function() {
    req(gbif_data())
    lng <- gbif_data()[[1]]$decimalLongitude; lat <- gbif_data()[[1]]$decimalLatitude
    simple_lft <- leafletProxy("occ_map") %>%
      addMarkers(lng = lng, lat = lat,
                 popup = paste("Lon:", round(lng, 2), "  |  ", "Lat:", round(lat, 2))
      ) %>%
      setView(lng = lng[1], lat = lat[1], zoom = 8)%>% # move to occurrence
    return(simple_lft)
}

## Add polygon or vector from shapefile
lft_geom <- function() {
  geom_vect <- geom_vect(); req(geom_vect)
  if (all(sf::st_is_valid(geom_vect))) {
  centro <- sf::st_centroid(st_union(geom_vect))[[1]]
    if (sf::st_geometry_type(geom_vect, by_geometry = F) %in% c("MULTIPOLYGON", "POLYGON")) {
      simple_lft <- leafletProxy("occ_map") %>%
        setView(lng = centro[1], lat = centro[2], zoom = 8) %>%
        addPolygons(data = geom_vect)
      return(simple_lft)
    } else {
      showModal(
        modalDialog(title = "", footer = modalButton("Ok"),
                    tags$h3("Choose a polygon vector"))
      )
    }
  }  else {
    showModal(
      modalDialog(title = "", footer = modalButton("Ok"),
                  tags$h3("Geometry not valid to be added"))
    )
  }

}
