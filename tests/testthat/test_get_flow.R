context("get_flow")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that(desc = "NIflows can be imported from a remote repository", code = {

  expect_works(capture.output(flow <- get_flow(repo_name = "neuroimaginador/test_flow",
                                               verbose = TRUE)))

})