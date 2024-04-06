test_that("test for nm_match_raster", {
  library(terra)

  env_layers_path <- paste0(system.file("extdata", package = "nimo"), "/env_layers")
  env_layers_path <- list.files(env_layers_path, full.names = TRUE)[1:2]

  target <- terra::rast(env_layers_path[1]) # reference raster to get properties from
  to_match <- terra::rast(env_layers_path[2]) # raster that will inherit properties

  new_dnw <- nm_match_raster(to_match = to_match, target = target)

  expect_true(class(new_dnw) == "SpatRaster")
  expect_error(
    nm_match_raster(to_match = to_match, target = target, method = "linear")
    )


  rm(env_layers_path, target, to_match, new_dnw)
})
