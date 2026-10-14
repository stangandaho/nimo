df <- read.csv(system.file("extdata", "Gyps_africanus_KNP.csv", package = "nimo"))
env <- terra::rast(system.file("extdata", "env_layers", c("dnw.tif", "dsm.tif"), package = "nimo"))
vars <- c("dnw", "dsm")
q <- function(x) suppressMessages(suppressWarnings(x))

test_that("Bioclim uncertainty reproduces the bootstrap by hand", {
  m <- q(nm_fit_bioclim(df, "pr_ab", vars, ".part"))
  u <- q(nm_uncertainty(m, df, projection_data = env, iteration = 3))
  expect_s4_class(u, "SpatRaster")
  expect_equal(names(u), "uncertainty")
  cells <- terra::as.data.frame(env, cells = TRUE, na.rm = TRUE)
  hand <- sapply(1:3, function(ii) {
    set.seed(ii)
    db <- q(flexsdm::part_random(df[, !startsWith(names(df), ".part")], "pr_ab",
                                 c(method = "boot", replicates = "1", proportion = "0.8")))
    db <- db[grepl("train", db$.part1), ]
    predict(nimo:::bioc_envelope(db[db$pr_ab == 1, vars]), cells)
  })
  expect_equal(terra::values(u)[cells$cell], apply(hand, 1, stats::sd), ignore_attr = TRUE)
})

test_that("flexsdm models match sdm_uncertainty; unused or missing layers give NA", {
  set.seed(1)
  m <- q(flexsdm::fit_glm(df, "pr_ab", vars, partition = ".part"))
  unused <- env[[1]]
  unused[1:5000] <- NA
  names(unused) <- "unused"
  u <- q(nm_uncertainty(m, df, projection_data = c(env, unused), iteration = 3))
  ref <- q(flexsdm::sdm_uncertainty(m, df, response = "pr_ab", projection_data = env,
                                    iteration = 3, n_cores = 1))
  ref <- terra::mask(ref, any(is.na(env)), maskvalues = 1)
  expect_equal(terra::values(u), terra::values(ref), ignore_attr = TRUE)
  expect_true(all(is.na(terra::values(u)[!stats::complete.cases(terra::values(env))])))
})

test_that("GBM models (not supported by sdm_uncertainty) work", {
  set.seed(1)
  m <- q(flexsdm::fit_gbm(df, "pr_ab", vars, partition = ".part"))
  u <- q(nm_uncertainty(m, df, projection_data = env, iteration = 3))
  expect_gt(terra::global(u, "max", na.rm = TRUE)[1, 1], 0)
  expect_error(nm_uncertainty(list(m), df, projection_data = env), "single fitted model")
  expect_error(nm_uncertainty(m, df, projection_data = env[[1]]), "not found")
})
