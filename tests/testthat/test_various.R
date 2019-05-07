context("Various")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("Two flows can be merged", {

  f <- function(...) {

    L <- list(...);

    unlist(L)

  }

  flow1 <- NIflow$new(name = "test", inputs = c(A, B))

  flow1$add(what = f, inputs = c(A, B), output = suma)
  flow1$add(inputs = C)
  flow1$add(what = f, inputs = c(B, C), output = suma2)
  flow1$add(what = f, inputs = c(suma, C), output = suma3)
  flow1$add(what = f, inputs = c(suma3, C), output = suma4)

  flow2 <- NIflow$new(name = "test2", inputs = c(A2, B2))

  flow2$add(what = f, inputs = c(A2, B2), output = suma_2)
  flow2$add(inputs = C2)
  flow2$add(what = f, inputs = c(B2, C2), output = suma2_2)
  flow2$add(what = f, inputs = c(suma_2, C2), output = suma3_2)
  flow2$add(what = f, inputs = c(suma3_2, C2), output = suma4_2)

  expect_works(flow3 <- merge(flow1, flow2))

  expect_works(flow3 %>% add(what = f, inputs = c(suma4, suma4_2), output = total))

})

test_that("A flow can be printed", {

  f <- function(...) {

    L <- list(...);

    unlist(L)

  }

  flow1 <- NIflow$new(name = "test", inputs = c(A, B))

  flow1$add(what = f, inputs = c(A, B), output = suma)
  flow1$add(inputs = C)
  flow1$add(what = f, inputs = c(B, C), output = suma2)
  flow1$add(what = f, inputs = c(suma, C), output = suma3)
  flow1$add(what = f, inputs = c(suma3, C), output = suma4)

  expect_works(print(flow1))

  expect_works(summary(flow1))

})

