context("get_flow")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that(desc = "NIflows can be imported from a remote repository", code = {

  # Get a remote repository
  expect_works(capture.output(flow <- get_flow(repo_name = "neuroimaginador/test_flow",
                                               verbose = TRUE)))

})

test_that(desc = "NIflow detects a flow is not available on a repository", code = {

  # # Non-exiting repository
  # expect_error(capture.output(flow <- get_flow(repo_name = "neuroimaginador/foo_flow",
  #                                              verbose = TRUE)))

})

test_that(desc = "The flows dir can be retrieved and set", code = {

  # Get current flows directory
  expect_works(my_dir <- get_flows_dir())
  expect_is(my_dir, "character")

  # Change temporarily to a new folder
  new_dir <- tempdir()
  expect_works(set_flows_dir(new_dir))
  expect_equivalent(get_flows_dir(), normalizePath(new_dir))

  # Back to the original folder
  expect_works(set_flows_dir(my_dir))

})