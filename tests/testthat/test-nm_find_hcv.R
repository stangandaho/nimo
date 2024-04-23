testthat::test_that("Test find highly correlated variables", {

  set.seed(012)
  age <- rpois(150, lambda = 25)
  heigth <- abs(rnorm(150, 1.1, 0.6))
  weigth <- abs(rnorm(150, 56.2, 11.4))
  glucose_rate <- runif(150, .2, .6)

  samp_data <- data.frame(age, heigth, weigth, glucose_rate)
  test_cor <- samp_data%>% cor()

  # highly correlated variables
  testthat::expect_equal(class(nm_find_hcv(x = test_cor)), "character")
  testthat::expect_equal(class(nm_find_hcv(x = test_cor, verbose = TRUE)), "character")


  samp_data$na_v <- sample(x = rep(c(1, NA, 5, 2, 5,9), 200), size = 150)
  test_cor <- samp_data%>% cor()

  expect_error(nm_find_hcv(x = test_cor))
  expect_error(nm_find_hcv(x = c(12, 45, 6)))

  rm(samp_data, test_cor)
})
