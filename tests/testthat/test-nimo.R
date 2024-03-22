testthat::test_that("Launch nimo", {
  testthat::expect_equal(class(nimo::nimo()), "shiny.appobj")
})
