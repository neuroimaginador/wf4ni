context("get_dependencies")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that(desc = "NIflow determines dependencies correctly", code = {

  # Basic function from neurobase package
  f <- function(I) neurobase::flip_img(I, x = TRUE)
  f2 <- function(I) {

    library(neurobase)

    flip_img(I, z = TRUE)

  }

  # Flow in which we add the function in two different ways
  flow <- NIflow$new(name = "test", inputs = T1)

  flow$add(what = f, inputs = T1, output = T1x)
  flow$add(what = neurobase::flip_img, inputs = T1, output = T1y, y = TRUE)
  flow$add(what = f2, inputs = T1, output = T1z)

  # Expect that the dependencies include the "neurobase" package
  expect_equivalent(flow$get_dependencies(), "neurobase")

})