#' Match two raster with different properties by transfering values
#' of a raster to a reference one (resampling).
#'
#' This function transfers values between two rasters that have a different
#' extent and/or resolution.
#'
#'@param to_match SpatRaster to match target
#'@param target SpatRaster with the properties that to_match should be transfered to
#'@param method Method used for estimating the new cell values. One of:
#' \itemize{
#'  \item _bilinear_ - the default: assigns a weighted average of the four nearest cells
#'  from the `to_match` raster to the cell of the `target` one. This is the fastest
#'  method that is appropriate for continuous rasters
#'  \item _near_: assigns the value of the nearest cell of the `to_match` raster to the
#'  cell of the `target` one. This is a fast simple technique that is usually suitable
#'  for resampling categorical rasters
#'  \item _cubic_: uses values of the 16 nearest cells of the `to_match` raster to
#'  determine the output cell value, applying third-order polynomial functions.
#'  Used for continuous rasters and results in a smoother surface compared to bilinear
#'  interpolation, but is computationally more demanding
#'  \item _cubicspline_: also uses values of the 16 nearest cells of the `to_match` raster
#'  to determine the output cell value, but applies cubic splines (piecewise third-order polynomial functions).
#'  Used for continuous rasters.
#'  \item _lanczos_: uses values of the 36 nearest cells of the `to_match` raster to determine the output cell value.
#'  Used for continuous rasters.
#'  \item _sum_: the weighted sum of all non-NA contributing grid cells.
#'  \item _min_, _q1_, _med_, _q3_, _max_, _average_, _mode_, _rms_: the minimum,
#'  first quartile, median, third quartile, maximum, mean, mode, or root-mean-square value
#'  of all non-NA contributing grid cells.
#' }
#'@param threads Default FALSE. If TRUE multiple threads are used (faster for large files)
#'@param save_file If TRUE, the output is saved
#'@param file_name character. Output file name (with format e.g .tif, .tiff, .TIF or .TIFF)
#'@param ... additional arguments for writing raster. See [terra::writeRaster()].
#'
#'@examples
#'# example code
#' original_rast <- rast(nrows=8, ncols=6, xmin=1, xmax=22, ymin=1, ymax=22)
#' values(original_rast) <- 1:ncell(original_rast)
#' to_match <- rast(nrows=15, ncols=20, xmin=4, xmax=15, ymin=3, ymax=25)
#' x <- nm_match_raster(to_match, original_rast, method="near")
#'
#'@references
#' - Hijmans, R. J., Bivand, R., Forner, K., Ooms, J., Pebesma, E., & Sumner, M. D. (2022). Package ‘terra.’ Maintainer: Vienna, Austria.
#' - Lovelace, R., Nowosad, J., & Muenchow, J. (2019). Geocomputation with R.
#' Chapman and Hall/CRC. [(https://doi.org/10.1201/9780203730058)](https://doi.org/10.1201/9780203730058)
#'
#'@export

nm_match_raster <- function(to_match, target, method = "bilinear", threads = FALSE,
                         save_file = FALSE, file_name = "", ...) {

  check_class <- c(class(target), class(to_match))
  if (all(check_class != "SpatRaster")) {
    stop("Both target and to_match must be SpatRaster object.")
  }

  if (save_file) {
    if (file_name == "") {
      file_name <- paste0(dirname(terra::sources(to_match)), "/",
                          strsplit(basename(terra::sources(to_match)), "\\.")[[1]][1],
                          "_matched.tif")
    }else{
      file_name <- paste0(getwd(), "/", file_name)
    }
  }

  if (save_file) {
    resampled <- terra::resample(x = to_match, y = target, method = method,
                                 filename = file_name, overwrite = TRUE, ...)
    message(paste("File saved to ", file_name))

    return(resampled)
  } else {
    return(terra::resample(x = to_match, y = target, method = method, ...))
  }
}


