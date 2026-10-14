df <- read.csv(system.file("extdata", "Gyps_africanus_KNP.csv", package = "nimo"))
vars <- c("dnw", "dsm")
q <- function(x) suppressMessages(suppressWarnings(x))

test_that("Domain models are predicted by nm_predict, also with extra layers", {
  env <- terra::rast(system.file("extdata", "env_layers", c("dnw.tif", "dsm.tif"), package = "nimo"))
  extra <- env[["dnw"]] * 2
  names(extra) <- "unused"
  m_dom <- q(flexsdm::fit_dom(df, "pr_ab", vars, partition = ".part", thr = "max_sens_spec"))

  p <- q(nm_predict(models = m_dom, pred = env, thr = "max_sens_spec"))
  expect_equal(names(p[[1]]), c("dom", "max_sens_spec"))
  ref <- q(flexsdm::sdm_predict(m_dom, env))[[1]]
  expect_equal(terra::values(p[[1]][[1]]), terra::values(ref), ignore_attr = TRUE)

  p2 <- q(nm_predict(models = m_dom, pred = c(env, extra)))
  expect_equal(terra::values(p2[[1]]), terra::values(ref), ignore_attr = TRUE)
})

test_that("nm_predict_df matches nm_predict", {
  env <- terra::rast(system.file("extdata", "env_layers", c("dnw.tif", "dsm.tif"), package = "nimo"))
  cells <- terra::as.data.frame(env, cells = TRUE, na.rm = TRUE)
  mods <- list(
    glm = q(flexsdm::fit_glm(df, "pr_ab", vars, partition = ".part")),
    raf = q(flexsdm::fit_raf(df, "pr_ab", vars, partition = ".part")),
    bioc = q(nm_fit_bioclim(df, "pr_ab", vars, ".part"))
  )
  for (m in mods) {
    r <- q(nm_predict(models = m, pred = env))[[1]]
    expect_equal(nimo:::nm_predict_df(m$model, cells[, vars]),
                 terra::values(r)[cells$cell], ignore_attr = TRUE)
  }
})

test_that("nm_varimp measures permutation importance", {
  set.seed(1)
  d <- df
  d$noise <- stats::rnorm(nrow(d))
  m_glm <- q(flexsdm::fit_glm(d, "pr_ab", c("lst", "ndvi", "noise"), partition = ".part",
                              thr = "max_sens_spec"))
  m_bioc <- q(nm_fit_bioclim(d, "pr_ab", c("lst", "ndvi", "noise"), ".part", thr = "max_sens_spec"))

  set.seed(2)
  vi <- nm_varimp(list(glm = m_glm, bioclim = m_bioc), d, n_sim = 5, metrics = c("AUC", "TSS"))
  expect_named(vi, c("model", "predictor", "metric", "baseline", "permuted",
                     "importance", "importance_sd", "relative"))
  expect_equal(nrow(vi), 2 * 3 * 2)
  expect_equal(vi$importance, vi$baseline - vi$permuted)
  rel <- tapply(vi$relative, paste(vi$model, vi$metric), sum)
  expect_true(all(abs(rel - 100) < 1e-8 | rel == 0))
  # land surface temperature drives the GLM much more than random noise
  auc <- vi[vi$model == "glm" & vi$metric == "AUC", ]
  expect_gt(auc$importance[auc$predictor == "lst"], auc$importance[auc$predictor == "noise"])

  # reproducible with a seed; single model; default labels and predictors
  set.seed(2)
  vi2 <- nm_varimp(list(glm = m_glm, bioclim = m_bioc), d, n_sim = 5, metrics = c("AUC", "TSS"))
  expect_identical(vi, vi2)
  vi1 <- nm_varimp(m_glm, d, n_sim = 2, metrics = "AUC")
  expect_equal(unique(vi1$model), "glm")
  expect_setequal(vi1$predictor, c("lst", "ndvi", "noise"))

  expect_s3_class(nm_varimp_plot(vi), "ggplot")
  expect_s3_class(nm_varimp_plot(vi, relative = TRUE), "ggplot")
  expect_error(nm_varimp(m_glm, d, metrics = "NOPE"), "Unknown metric")
  expect_error(nm_varimp(m_glm, d[d$pr_ab == 1, ]), "presences")
})
