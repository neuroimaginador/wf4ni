context("NIflow")

expect_works <- function(object) testthat::expect_error(object, NA)

# Download external nifti file
t1_file <- tempfile(fileext = ".nii.gz")
download.file(url = "http://www.jannin.org/mritemplate/data/Template-T1-U8-RALPFH-BR.nii.gz",
              destfile = t1_file, quiet = TRUE)

test_that("NIflow initializes as expected", {

  # Create flow
  flow <- NIflow$new(name = "foo", inputs = c("A", "B"))

  # Expectations
  # Class
  expect_is(flow, "NIflow")

  # Correct name
  expect_true(flow$name() == "foo")

  # Retrieving inputs
  expect_identical(sort(flow$get_inputs()), c("A", "B"))

  # No outputs at the beginning
  expect_identical(setdiff(flow$get_outputs(), flow$get_inputs()), character(0))

})

test_that("NIflow adds inputs", {

  # Create flow
  flow <- NIflow$new(name = "foo", inputs = c("A", "B"))
  original_inputs <- flow$get_inputs()

  # Add a new input
  flow$add(inputs = "C")

  # Expect we have the new input added
  expect_identical(setdiff(flow$get_inputs(), original_inputs), "C")

})

test_that("A NIflow can do basic operations (plot and subset)", {

  f <- function(...) {

    L <- list(...);

    unlist(L)

  }

  flow <- NIflow$new(name = "test", inputs = c(A, B))

  flow$add(what = f, inputs = c(A, B), output = suma)
  flow$add(inputs = C)
  flow$add(what = f, inputs = c(B, C), output = suma2)
  flow$add(what = f, inputs = c(suma, C), output = suma3)
  flow$add(what = f, inputs = c(suma3, C), output = suma4)

  # Plot
  expect_works(flow$plot())

  # Subset
  expect_works(new_flow <- flow$subset(outputs = "suma3"))
  expect_works(new_flow$plot())

  # Memory used
  expect_works(flow$memory_used())

  # Deep Clone
  expect_works(new_flow <- flow$deep_clone())

})


test_that(desc = "A NIflow can be executed", code = {

  f <- function(...) {

    L <- list(...);

    unlist(L)

  }

  flow <- NIflow$new(name = "test", inputs = c(A, B))

  flow$add(what = f, inputs = c(A, B), output = suma)
  flow$add(inputs = C)
  flow$add(what = f, inputs = c(B, C), output = suma2)
  flow$add(what = f, inputs = c(suma, C), output = suma3)
  flow$add(what = f, inputs = c(suma3, C), output = suma4)

  # Execute the flow
  expect_works(res <- flow$execute(inputs = list(A = 1, B = 2, C = 3),
                                   desired_outputs = c(suma, suma2, suma3)))

  # Should return a list with named components
  expect_is(res, "list")
  expect_named(res, expected = c("suma", "suma2", "suma3"))

  # The same for function "run" instead of "execute"
  expect_works(res <- flow$run(inputs = list(A = 1, B = 2, C = 3),
                               desired_outputs = c(suma, suma2, suma3)))

  expect_is(res, "list")
  expect_named(res, expected = c("suma", "suma2", "suma3"))

  # Unavailable outputs should warn the user
  expect_warning(flow$execute(inputs = list(A = 1, B = 2, c = 3),
                              desired_outputs = suma6))

})


test_that(desc = "A NIflow can be exported and imported", code = {

  f <- function(...) {

    L <- list(...);

    unlist(L)

  }

  flow <- NIflow$new(name = "test", inputs = c(A, B))

  flow$add(what = f, inputs = c(A, B), output = suma)
  flow$add(inputs = C)
  flow$add(what = f, inputs = c(B, C), output = suma2)
  flow$add(what = f, inputs = c(suma, C), output = suma3)
  flow$add(what = f, inputs = c(suma3, C), output = suma4)

  # Save and load
  path <- tempdir()
  file_prefix <- basename(tempfile())
  expect_works(flow$save(path = path, file_prefix = file_prefix))

  expect_works(flow$load(filename = file.path(path, paste0(file_prefix, ".zip"))))

})

test_that(desc = "NIflow get/sets", code = {

  f <- function(...) {

    L <- list(...);

    unlist(L)

  }

  flow <- NIflow$new(name = "test", inputs = c(A, B))

  flow$add(what = f, inputs = c(A, B), output = suma)
  flow$add(inputs = C)
  flow$add(what = f, inputs = c(B, C), output = suma2)
  flow$add(what = f, inputs = c(suma, C), output = suma3)
  flow$add(what = f, inputs = c(suma3, C), output = suma4)

  # Change name
  expect_works(flow$set_name("test2"))
  expect_equivalent(flow$name(), expected = "test2")

  # Get process by id
  expect_works(f2 <- flow$get_process(suma))
  expect_identical(object = f2, expected = f)

  # Get and check dependencies
  expect_works(flow$get_dependencies())
  expect_works(flow$check_dependencies())

  # Replace function
  f3 <- function(...) {L <- list(...); unlist(rev(L))}

  expect_works(flow$replace(output = suma, with = f3))
  expect_works(f2 <- flow$get_process(suma))
  expect_identical(object = f2, expected = f2)

})

test_that(desc = "A Niflow logs things", code = {

  f <- function(...) {

    L <- list(...);

    unlist(L)

  }

  flow <- NIflow$new(name = "test", inputs = c(A, B))

  flow$add(what = f, inputs = c(A, B), output = suma)
  flow$add(inputs = C)
  flow$add(what = f, inputs = c(B, C), output = suma2)
  flow$add(what = f, inputs = c(suma, C), output = suma3)
  flow$add(what = f, inputs = c(suma3, C), output = suma4)

  res <- flow$execute(inputs = list(A = 1, B = 2, C = 3),
                      desired_outputs = c(suma, suma2, suma3))

  # Printing log
  expect_works(capture.output(flow$print_log()))
  expect_works(capture.output(flow$errors()))
  expect_works(capture.output(flow$warnings()))

  # Adding to log
  flow$log(level = "DEBUG", message = "TEST")

  # Saving log
  path <- tempfile()
  expect_works(flow$save_log(filename = path))
  expect_true(file.exists(path))

})

test_that(desc = "A NIflow has a graph representation", code = {

  f <- function(...) {

    L <- list(...);

    unlist(L)

  }

  flow <- NIflow$new(name = "test", inputs = c(A, B))

  flow$add(what = f, inputs = c(A, B), output = suma)
  flow$add(inputs = C)
  flow$add(what = f, inputs = c(B, C), output = suma2)
  flow$add(what = f, inputs = c(suma, C), output = suma3)
  flow$add(what = f, inputs = c(suma3, C), output = suma4)

  # Obtain the graph
  expect_works(g <- flow$graph())

  # Is it actually a graph?
  expect_is(g, "igraph")

})

test_that(desc = "A NIflow accepts parameterizable functions", code = {

  # Parameter b in f1 can be adjusted
  f1 <- function(v, b = 2) {v + b}
  f2 <- function(v) {2 * v}

  flow <- NIflow$new(name = "test", inputs = c(A, B))

  # It is b = 0
  flow$add(what = f1, inputs = A, output = F1, b = 0)
  flow$add(what = f2, inputs = A, output = F2)
  flow$add(what = f1, inputs = A, output = F15, b = 5) # b = 5 in this case

  expect_works(res <- flow$execute(inputs = list(A = 1), desired_outputs = c(F1, F2, F15)))

  # Expected results
  expect_is(res, "list")
  expect_named(res, expected = c("F1", "F2", "F15"))
  expect_equivalent(res, expected =  list(F1 = 1, F2 = 2, F15 = 6))

})

test_that(desc = "A NIflow accepts strings as input names", code = {

  # Use of programatically defined variable names
  a <- "A1"
  b <- "B1"
  expect_works(flow <- NIflow$new(name = "test", inputs = c(a, b)))

  expect_equivalent(flow$get_inputs(), expected = c("A1", "B1"))

  # Use of mixed variable names
  expect_works(flow <- NIflow$new(name = "test", inputs = c(a, r)))

  expect_equivalent(flow$get_inputs(), expected = c("A1", "r"))

})

test_that(desc = "A NIflow can be exported to an R package", code = {

  # Auxiliary functions
  f <- function(...) {

    L <- list(...);

    unlist(L)

  }

  # Test flow
  flow <- NIflow$new(name = "test", inputs = c(A, B))

  flow$add(what = f, inputs = c(A, B), output = suma)
  flow$add(inputs = C)
  flow$add(what = f, inputs = c(B, C), output = suma2)
  flow$add(what = f, inputs = c(suma, C), output = suma3)
  flow$add(what = f, inputs = c(suma3, C), output = suma4)

  # Exporting to a package
  path <- tempdir()
  expect_works(flow$to_package(path = path))

  # Was the export successful?
  expect_true(file.exists(file.path(path, flow$name())))
  expect_works(res <- devtools::as.package(x = file.path(path, flow$name())))
  expect_is(res, "package")

})

test_that(desc = "NIflow can read NIFTI(gz) files as input", code = {


  # Sample flow
  flow <- NIflow$new(name = "test", inputs = T1)
  flow$add(what = function(I) {-I}, inputs = T1, output = negT1)

  # Input is a filename (.nii.gz)
  expect_works(res <- flow$execute(inputs = list(T1 = t1_file),
                                   desired_outputs = negT1))

  # Is the result as expected?
  expect_is(res, "list")
  expect_named(res, expected = c("negT1"))
  expect_is(res$negT1, "array")

})

test_that(desc = "NIflow can read RDS files as input", code = {


  # Rewrite input as RDS file
  nifti_img <- neurobase::readnii(t1_file)
  tmp <- tempfile(fileext = ".rds")
  nifti_img %>% as.array() %>% saveRDS(file = tmp)

  # Sample flow
  flow <- NIflow$new(name = "test", inputs = T1)
  flow$add(what = function(I) {-I}, inputs = T1, output = negT1)

  # Input is a filename (.rds)
  expect_works(res <- flow$execute(inputs = list(T1 = tmp),
                                   desired_outputs = negT1))

  # Is the result as expected?
  expect_is(res, "list")
  expect_named(res, expected = c("negT1"))
  expect_is(res$negT1, "array")

})

test_that(desc = "NIflow can read uncompressed NIFTI files as input", code = {


  # Rewrite input as .nii (uncompressed) file.
  nifti_img <- neurobase::readnii(t1_file)
  tmp <- tempfile(fileext = "")
  oro.nifti::writeNIfTI(nifti_img, filename = tmp, gzipped = FALSE)

  # Sample flow
  flow <- NIflow$new(name = "test", inputs = T1)
  flow$add(what = function(I) {-I}, inputs = T1, output = negT1)

  # Input is a filename (.nii)
  expect_works(res <- flow$execute(inputs = list(T1 = paste0(tmp, ".nii")),
                                   desired_outputs = negT1))

  # Is the result as expected?
  expect_is(res, "list")
  expect_named(res, expected = c("negT1"))
  expect_is(res$negT1, "array")

})