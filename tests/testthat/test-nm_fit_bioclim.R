test_that("test different function setting ", {
  data("abies")

  # Using k-fold partition method
  abies2 <- part_random(
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
  abies2 <- part_random(
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
  expect_length(max_t1, 4)

  # What about no predictors? Does not work
  expect_error(nm_fit_bioclim(
    data = abies2,
    response = "pr_ab",
    partition = ".part",
    thr = c("max_sens_spec", "equal_sens_spec")
  ))
})
