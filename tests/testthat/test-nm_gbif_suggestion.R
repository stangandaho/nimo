testthat::test_that("Get Gyps africanus suggestion", {

  # get data frame for Gyps suggestion
  gyps <- nimo::nm_gbif_suggestion(query = "Gyps")
  testthat::expect_equal(class(gyps), "data.frame")

  # What about no predictors? Does not work
  testthat::expect_error(nm_gbif_suggestion(time_out = 5))
})
