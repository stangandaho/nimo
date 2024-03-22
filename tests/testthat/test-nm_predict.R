test_that("test for fit_ function family", {
  require(dplyr)
  require(terra)
  require(flexsdm)

  # Environmental variables
  somevar <- system.file("external/somevar.tif", package = "flexsdm") %>% terra::rast()
  regions <- system.file("external/regions.tif", package = "flexsdm") %>% terra::rast()
  # levels(regions) <- unique(regions)
  somevar <- terra::rast(x = list(regions, somevar))
  somevar2 <- rast(list(somevar, somevar$category))
  names(somevar2)[6] <- "category2"

  # Species occurrences
  data("spp")
  set.seed(1)
  spp_ <- spp %>%
    dplyr::filter(species == "sp2") %>%
    sdm_extract(
      data = .,
      x = "x",
      y = "y",
      env_layer = somevar2,
      variables = names(somevar2),
      filter_na = TRUE
    ) %>%
    part_random(
      data = .,
      pr_ab = "pr_ab",
      method = c(method = "kfold", folds = 3)
    )


  # gam
  bioc <- nm_fit_bioclim(
    data = spp_,
    response = "pr_ab",
    predictors = c("CFP_1", "CFP_2", "CFP_3", "CFP_4"),
    partition = ".part",
    thr = c("max_sens_spec")
  )

  p <- nm_predict(
    models = bioc,
    pred = somevar,
    thr = NULL,
    con_thr = FALSE
  )

  expect_true(class(p[[1]]) == "SpatRaster")
  expect_equal(terra::nlyr(p[[1]]), 1)
  rm(p)
})
