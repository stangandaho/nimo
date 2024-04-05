test_that("test for nm_extract", {
  require(terra)

  wbv_path <- paste0(system.file("extdata", package = "nimo"), "/WbV_subset_occ_KNP.csv")
  wbv_df <- read.csv(wbv_path)

  env_layers_path <- paste0(system.file("extdata", package = "nimo"), "/env_layers")
  env_layers <- terra::rast(list.files(env_layers_path, full.names = TRUE))

  extracted <- nimo::nm_extract(data = wbv_df,
                         longitude = "decimalLongitude",
                         latitude = "decimalLatitude",
                         env_layer = env_layers)

  expect_true(all(class(extracted) %in% c("tbl_df", "tbl", "data.frame")))
  expect_error(nm_extract(data = wbv_df,
                          longitude = "decimalLongitude",
                          latitude = "decimalLatitude",
                          env_layer = env_layers,
                          current_crs = "geo"))
  rm(extracted, wbv_path, wbv_df, env_layers_path, env_layers)
})
