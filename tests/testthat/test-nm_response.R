df <- read.csv(system.file("extdata", "Gyps_africanus_KNP.csv", package = "nimo"))
vars <- c("dnw", "dsm", "lst", "ndvi")
q <- function(x) suppressMessages(suppressWarnings(x))

test_that("marginal curves match flexsdm::data_pdp", {
  set.seed(1)
  mods <- list(glm = q(flexsdm::fit_glm(df, "pr_ab", vars, partition = ".part")),
               raf = q(flexsdm::fit_raf(df, "pr_ab", vars, partition = ".part")))
  rc <- nm_response(mods, df, resolution = 20)
  expect_named(rc, c("model", "predictor", "value", "level", "suitability"))
  expect_equal(nrow(rc), 2 * 4 * 20)
  for (a in names(mods)) {
    fx <- q(flexsdm::data_pdp(mods[[a]]$model, predictors = "lst", resolution = 20,
                              training_data = df))$pdpdata
    expect_equal(rc$suitability[rc$model == a & rc$predictor == "lst"], fx$Suitability,
                 ignore_attr = TRUE)
  }
})

test_that("partial dependence averages predictions; works for Bioclim and Domain", {
  m_bioc <- q(nm_fit_bioclim(df, "pr_ab", vars, ".part"))
  m_dom <- q(flexsdm::fit_dom(df, "pr_ab", vars, partition = ".part"))
  rc <- nm_response(list(bioclim = m_bioc, domain = m_dom), df, predictors = "ndvi",
                    type = "partial", resolution = 5)
  expect_equal(attr(rc, "type"), "partial")
  expect_setequal(rc$model, c("bioclim", "domain"))
  d2 <- df
  d2$ndvi <- rc$value[2]
  expect_equal(rc$suitability[2], mean(predict(m_bioc$model, d2)))
  expect_true(all(rc$suitability >= 0 & rc$suitability <= 1))

  # n_max subsamples the records
  set.seed(3)
  small <- nm_response(m_bioc, df, predictors = "ndvi", type = "partial", resolution = 5, n_max = 50)
  expect_equal(nrow(small), 5)

  expect_s3_class(nm_response_plot(rc, data = df), "ggplot")
  expect_s3_class(nm_response_plot(rc), "ggplot")
  expect_error(nm_response(m_bioc, df[, c("pr_ab", "dnw")]), "not found")
})

test_that("categorical predictors give one value per level", {
  d <- df
  d$zone <- factor(ifelse(d$lst > stats::median(d$lst), "hot", "cool"))
  m <- q(flexsdm::fit_glm(d, "pr_ab", c("dnw", "dsm"), predictors_f = "zone", partition = ".part"))
  rc <- nm_response(m, d, predictors = "zone")
  expect_equal(sort(rc$level), c("cool", "hot"))
  expect_true(all(is.na(rc$value)))
  expect_s3_class(nm_response_plot(rc), "ggplot")
})

test_that("two-predictor surfaces match flexsdm::data_bpdp and support partial dependence", {
  set.seed(1)
  m_glm <- q(flexsdm::fit_glm(df, "pr_ab", vars, partition = ".part"))
  m_bioc <- q(nm_fit_bioclim(df, "pr_ab", vars, ".part"))
  rs <- nm_response2d(list(glm = m_glm, bioclim = m_bioc), df, c("lst", "ndvi"), resolution = 15)
  expect_named(rs, c("model", "lst", "ndvi", "suitability"))
  expect_equal(nrow(rs), 2 * 15^2)
  fx <- q(flexsdm::data_bpdp(m_glm$model, predictors = c("lst", "ndvi"), resolution = 15,
                             training_data = df))$pspdata
  j <- merge(as.data.frame(rs[rs$model == "glm", ]), fx, by = c("lst", "ndvi"))
  expect_equal(nrow(j), 15^2)
  expect_equal(j$suitability, j$Suitability)
  expect_gt(nrow(attr(rs, "boundary")), 2)

  rp <- nm_response2d(m_bioc, df, c("lst", "ndvi"), type = "partial", resolution = 3, n_max = 1000)
  d2 <- df
  d2$lst <- rp$lst[5]
  d2$ndvi <- rp$ndvi[5]
  expect_equal(rp$suitability[5], mean(predict(m_bioc$model, d2)))

  expect_s3_class(nm_response2d_plot(rs), "ggplot")
  expect_error(nm_response2d(m_glm, df, "lst"), "two predictor")
  m_two <- q(nm_fit_bioclim(df, "pr_ab", c("dnw", "dsm"), ".part"))
  expect_error(nm_response2d(m_two, df, c("lst", "ndvi")), "does not use")
})
