#' Extract raster data values based on point coordinates (longitude, latitude)
#'
#' @param data data.frame. Database with species presence, presence-absence,
#' or pseudo-absence records with longitude and latitude coordinates.
#' @param longitude character. Column name with spatial longitude coordinates.
#' @param latitude character. Column name with spatial latitude coordinates.
#' @param env_layer SpatRaster. Raster or raster stack.
#' @param current_crs character. Current coordinate reference system of coordinates. One of:
#' * `geographic`: Apllicable for longitude and latitude in decimal
#' (e.g 31.796406, -24.536200)
#' * `projected`: Applicable for longitude and latitude in projected
#' system (e.g 7280635, 982148 - UTM zone 35S)
#' @param variables character. Vector with the raster names to extract value from.
#' If NULL (default) the function will return data for all raster.
#' @param filter_na logical. If filter_na = TRUE (default), the rows with NA values
#' for any of the raster are removed from the returned tibble.
#'
#' @return tibble with original data bound with extracted data
#'
#'
#' @examples
#'  library(terra)
#'  wbv_path <- paste0(system.file("extdata", package = "nimo"), "/WbV_subset_occ_KNP.csv")
#'  wbv_df <- read.csv(wbv_path)
#'
#'  env_layers_path <- paste0(system.file("extdata", package = "nimo"), "/env_layers")
#'  env_layers <- terra::rast(list.files(env_layers_path, full.names = TRUE)[c(1,3)])
#'
#' extracted <- nm_extract(data = wbv_df, longitude = "decimalLongitude",
#' latitude = "decimalLatitude", env_layer = env_layers)
#'
#' @export
nm_extract <- function (data,
                        longitude,
                        latitude,
                        env_layer,
                        current_crs = "geographic",
                        variables = NULL,
                        filter_na = TRUE)
{
  if (is.null(variables)) {
    variables <- names(env_layer)
  }
  data <- data %>%
    dplyr::filter(!is.na(!!dplyr::sym(longitude))) %>%
    dplyr::filter(!is.na(!!dplyr::sym(latitude)))

  if (! current_crs %in% c("geographic", "projected")) {
    stop("The current crs must be one of 'geographic' and 'projected'")
  }
  if(current_crs == "geographic"){
    coord_rs <- 4326
  }else{
    coord_rs <- terra::crs(env_layer)
  }
  sp_data <- sf::st_as_sf(data, coords = c(longitude, latitude), crs = coord_rs) %>%
    sf::st_transform(crs = terra::crs(env_layer))
  extract_data <- dplyr::tibble(data,
                                terra::extract(env_layer[[variables]], sp_data, cells = FALSE) %>%
                                  dplyr::select({{variables}}))
  if (filter_na) {
    complete_vec <- stats::complete.cases(extract_data[,
                                                       variables])
    if (sum(!complete_vec) > 0) {
      message(sum(!complete_vec), " rows were excluded from database because NAs were found")
      extract_data <- extract_data %>% dplyr::filter(complete_vec)
    }
  }
  return(extract_data)
}


