
llf <- function() {
  simple_lft <- leaflet::leaflet() %>%
    setView(lng = 1.8, lat = 11, zoom = 8)%>%
    addTiles() %>%
    addMiniMap(position = "bottomleft", height = 100, width = 100) %>%
    addDrawToolbar(targetGroup='draw', markerOptions = FALSE, polylineOptions = FALSE,
                   editOptions = editToolbarOptions(), circleMarkerOptions = FALSE,
                   circleOptions = FALSE, rectangleOptions = FALSE, singleFeature = TRUE) %>%
    addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "meters",
      primaryAreaUnit = "sqmeters",
      activeColor = "#3D535D",
      completedColor = "#7D4479")
  return(simple_lft)
}


lft_proxy <- function() {
    req(gbif_data())
    lng <- gbif_data()$decimalLongitude; lat <- gbif_data()$decimalLatitude
    simple_lft <-   leafletProxy("occ_map") %>%
      addMarkers(lng = lng, lat = lat,
                 popup = paste("Lon:", round(gbif_data()$decimalLongitude, 2), "  |  ",
                               "Lat:", round(gbif_data()$decimalLatitude, 2))
      )
    return(simple_lft)
}

lft_geom <- function() {
  req(geom())
  simple_lft <-   leafletProxy("occ_map") %>%
    addPolygons(data = geom())
  return(simple_lft)
}
