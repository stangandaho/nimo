testthat::test_that("Test find highly correlated variables", {

  set.seed(012)
  age <- rpois(150, lambda = 25)
  heigth <- abs(rnorm(150, 1.1, 0.6))
  weigth <- abs(rnorm(150, 56.2, 11.4))
  glucose_rate <- runif(150, .2, .6)

  test_data <- data.frame(age, heigth, weigth, glucose_rate) %>%
    cor()

  # highly correlated variables
  testthat::expect_equal(class(nm_find_hcv(x = test_data)), "character")
})
