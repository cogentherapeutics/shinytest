context("Exported values")

app <- shinyapp$new(test_path("apps/test-exports/"))

test_that("Exported values", {
  x <- app$get_all_values()
  expect_identical(x$exports$x, 1)
  expect_identical(x$exports$y, 2)

  app$set_inputs(inc = "click")
  app$set_inputs(inc = "click")

  x <- app$get_all_values()
  expect_identical(x$exports$x, 3)
  expect_identical(x$exports$y, 4)
})