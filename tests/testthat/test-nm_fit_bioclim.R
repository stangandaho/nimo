test_that("test different function setting ", {
  data("abies", package = "flexsdm")

  # Using k-fold partition method
  abies2 <- flexsdm::part_random(
    data = abies,
    pr_ab = "pr_ab",
    method = c(method = "kfold", folds = 3)
  )

  # generating background data
  bg <- abies2
  bg$pr_ab <- 0

  bioc1 <- nm_fit_bioclim(
    data = abies2,
    response = "pr_ab",
    predictors = c("aet", "ppt_jja", "pH", "awc", "depth"),
    partition = ".part",
    thr = c("max_sens_spec", "equal_sens_spec")
  )

  expect_equal(class(bioc1), "list")
  expect_length(bioc1, 4)

  # Using bootstrap partition method
  abies2 <- flexsdm::part_random(
    data = abies,
    pr_ab = "pr_ab",
    method = c(method = "boot", replicates = 5, proportion = 0.7)
  )

  # generating background data
  bg <- abies2
  bg$pr_ab <- 0

  bioc2 <- nm_fit_bioclim(
    data = abies2,
    response = "pr_ab",
    predictors = c("ppt_jja", "pH", "awc"),
    partition = ".part",
    thr = c(type = c("lpt", "max_sens_spec", "sensitivity"), sens = "0.8")
  )

  expect_equal(class(bioc2), "list")
  expect_length(bioc2, 4)

  # What about no predictors? Does not work
  expect_error(nm_fit_bioclim(
    data = abies2,
    response = "pr_ab",
    partition = ".part",
    thr = c("max_sens_spec", "equal_sens_spec")
  ))
})

test_that("native Bioclim matches dismo::bioclim", {
  skip_if_not_installed("dismo")
  df <- read.csv(system.file("extdata", "Gyps_africanus_KNP.csv", package = "nimo"))
  vars <- c("dnw", "dsm", "lst", "ndvi")
  pres <- df[df$pr_ab == 1, vars]
  d <- dismo::bioclim(as.matrix(pres))
  for (tl in c("both", "low", "high")) {
    m <- nimo:::bioc_envelope(pres, tails = tl)
    expect_equal(predict(m, df), dismo::predict(d, df[, vars], tails = rep(tl, 4)))
  }
})

test_that("Bioclim options, partial scores and diagnostics", {
  df <- read.csv(system.file("extdata", "Gyps_africanus_KNP.csv", package = "nimo"))
  vars <- c("dnw", "dsm", "lst", "ndvi")
  m <- suppressMessages(nm_fit_bioclim(df, "pr_ab", vars, ".part", thr = "max_sens_spec"))
  expect_s3_class(m$model, "nm_bioclim")
  expect_named(m, c("model", "predictors", "performance", "data_ens"))
  expect_named(m$data_ens, c("rnames", "replicates", "part", "pr_ab", "pred"))
  expect_equal(sort(unique(m$performance$threshold)), "max_sens_spec")

  partial <- predict(m$model, df, type = "partial")
  expect_equal(colnames(partial), vars)
  expect_equal(apply(partial, 1, min), predict(m$model, df))
  expect_true(all(partial >= 0 & partial <= 1))

  # aggregation: min <= gmean <= mean
  mg <- suppressMessages(nm_fit_bioclim(df, "pr_ab", vars, ".part", aggregation = "gmean"))
  mm <- suppressMessages(nm_fit_bioclim(df, "pr_ab", vars, ".part", aggregation = "mean"))
  p_min <- predict(m$model, df); p_g <- predict(mg$model, df); p_m <- predict(mm$model, df)
  expect_true(all(p_min <= p_g + 1e-12) && all(p_g <= p_m + 1e-12))

  # per-variable tails
  mt <- suppressMessages(nm_fit_bioclim(df, "pr_ab", vars, ".part",
                                        tails = c(dnw = "both", dsm = "both", lst = "both", ndvi = "low")))
  expect_equal(unname(mt$model$tails), c("both", "both", "both", "low"))
  high_ndvi <- df[1, ]; high_ndvi$ndvi <- max(df$ndvi[df$pr_ab == 1])
  expect_equal(unname(predict(mt$model, high_ndvi, type = "partial")[, "ndvi"]), 1)

  # envelope classes on tabular data
  env_cls <- nm_bioclim_envelope(m, df[df$pr_ab == 1, ])
  expect_true(all(env_cls %in% c("marginal", "core")))
  expect_s3_class(nm_bioclim_plot(m, "lst", "ndvi", data = df), "ggplot")

  # informative errors
  expect_error(nm_fit_bioclim(df, "pr_ab", c("dnw", "nope"), ".part"), "not found")
  expect_error(nm_fit_bioclim(df[df$pr_ab == 1, ], "pr_ab", vars, ".part"), "absences")
  expect_error(nm_fit_bioclim(df, "pr_ab", vars, ".part", tails = "middle"), "tails")
})

test_that("Bioclim raster predictions, limiting factor and chunked nm_predict", {
  df <- read.csv(system.file("extdata", "Gyps_africanus_KNP.csv", package = "nimo"))
  env <- terra::rast(system.file("extdata", "env_layers", c("dnw.tif", "dsm.tif"), package = "nimo"))
  m <- suppressMessages(nm_fit_bioclim(df, "pr_ab", c("dnw", "dsm"), ".part", thr = "max_sens_spec"))

  r <- predict(m$model, env)
  expect_s4_class(r, "SpatRaster")
  p1 <- suppressMessages(nm_predict(models = m, pred = env))
  p3 <- suppressMessages(nm_predict(models = m, pred = env, nchunk = 3))
  expect_equal(terra::values(p1[[1]]), terra::values(r), ignore_attr = TRUE)
  expect_equal(terra::values(p3[[1]]), terra::values(r), ignore_attr = TRUE)

  lim <- nm_bioclim_limiting(m, env)
  expect_true(terra::is.factor(lim))
  expect_setequal(terra::levels(lim)[[1]]$limiting, c("dnw", "dsm"))

  ev <- nm_bioclim_envelope(m, env)
  expect_true(all(stats::na.omit(terra::values(ev)) %in% 0:2))
})
