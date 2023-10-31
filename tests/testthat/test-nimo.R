testthat::test_that("Launch nimo", {
  expect_equal(class(nimo::nimo()), "shiny.appobj")
})
