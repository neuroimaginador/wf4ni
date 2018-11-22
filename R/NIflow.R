#' NIflow Class
#'
#' @docType class
#' @importFrom R6 R6Class
#'
#' @export
#' @keywords data
#'
#' @return Object of \code{\link{R6Class}} and \code{NIflow}.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @examples
#' NIflow$new()
#'
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method follow the corresponding link. }
#'
#'   \item{\code{initialize(name, inputs = list())}}{Create a new flow with the given name and inputs. Documented in \link{NIflow.initialize}.}
#'
#'   \item{\code{name()}}{Returns the name of the flow. Documented in \link{NIflow.name}.}
#'
#'   \item{\code{get_private()}}{Returns the private environment inside the class object. Documented in \link{NIflow.get_private}.}
#'
#'   \item{\code{set_name(new_name)}}{Changes the name of the flow. Documented in \link{NIflow.set_name}.}
#'
#'   \item{\code{get_inputs()}}{Returns the inputs of the flow. Documented in \link{NIflow.get_inputs}.}
#'
#'   \item{\code{get_outputs()}}{Returns the outputs of the flow. Documented in \link{NIflow.get_outputs}.}
#'
#'   \item{\code{get_required_inputs(outputs)}}{Returns the inputs required to compute given outputs. Documented in \link{NIflow.get_required_inputs}.}
#'
#'   \item{\code{get_process(output)}}{Returns the function that computes the given output. Documented in \link{NIflow.get_process}.}
#'
#'   \item{\code{get_dependencies()}}{Returns the list of package dependencies of the flow. Documented in \link{NIflow.get_dependencies}.}
#'
#'   \item{\code{check_dependencies()}}{Checks if all flow dependencies are installed. Documented in \link{NIflow.check_dependencies}.}
#'
#'   \item{\code{replace(output, with)}}{Replace a function that computes an output
#'   with another function. Documented in \link{NIflow.replace}.}
#'
#'   \item{\code{add(what = NULL, inputs = NULL, output = NULL, ...)}}{Adds a function to the flow,
#'   that transforms the given inputs into the given output. Documented in \link{NIflow.add}.}
#'
#'   \item{\code{execute(inputs = list(), desired_outputs = NULL, initialize_outputs = TRUE, ...)}}{Execute the flow to obtain some outputs, given input files. Documented in \link{NIflow.execute}.}
#'
#'   \item{\code{run(...)}}{Just a wrapper for \code{execute}. Documented in \link{NIflow.run}.}
#'
#'   \item{\code{log(level = c("DEBUG", "INFO", "WARNING", "ERROR"), message = '...')}}{Add a message to the flow log, including the level of the message and a timestamp. Documented in \link{NIflow.log}.}
#'
#'   \item{\code{print_log(level = c("DEBUG", "WARNING", "INFO", "ERROR"))}}{Prints the log of the flow. Documented in \link{NIflow.print_log}.}
#'
#'   \item{\code{errors()}}{Just prints the errors in the flow log. Documented in \link{NIflow.errors}.}
#'
#'   \item{\code{warnings()}}{Just prints the warnings in the flow log. Documented in \link{NIflow.warnings}.}
#'
#'   \item{\code{save_log(filename, level = c("DEBUG", "WARNING", "INFO", "ERROR"))}}{Saves a the flow log to file. Documented in \link{NIflow.save_log}.}
#'
#'   \item{\code{graph()}}{Returns the graph of the flow. Documented in \link{NIflow.graph}.}
#'
#'   \item{\code{plot()}}{Plots the graph of the flow. Documented in \link{NIflow.plot}.}
#'
#'   \item{\code{memory_used()}}{Total memory used by the flow, including its internal variables. Documented in \link{NIflow.memory_used}.}
#'
#'   \item{\code{save(path = tempdir(), file_prefix = self$name())}}{Save the flow to disk. Documented in \link{NIflow.save}.}
#'
#'   \item{\code{load(filename)}}{Load a flow from disk. Documented in \link{NIflow.load}.}
#'
#'   \item{\code{deep_clone()}}{Clone a flow. Documented in \link{NIflow.deep_clone}.}
#'
#'   \item{\code{subset(outputs)}}{Take a subset of the flow, for given outputs. Documented in \link{NIflow.subset}.}
#'
#'   \item{\code{to_package(path, package_name = self$name())}}{Creates a package with functions provided by the execution of the flow. Documented in \link{NIflow.to_package}.}
#'
#'  }
#'
NIflow <- R6::R6Class(

  classname = "NIflow",


  public = list(

    initialize = function(name = "", inputs = list()) {

      E <- try(eval(inputs), silent = TRUE)

      if (!inherits(E, "try-error")) {

        inputs <- E

      } else {

        expr <- substitute(inputs)
        inputs <- as.character(expr)

        if (class(expr) == "call")
          inputs <- inputs[-1]

        env <- parent.frame(2)
        inputs <- inputs %>% .search_names(envir = env)

      }

      flow_env <- .create_flow(name = name, inputs = inputs)

      self$.__enclos_env__$private <- flow_env

    },

    name = function() {

      private$name

    },

    get_private = function() {

      return(self$.__enclos_env__$private)

    },

    set_name = function(new_name) {

      private$name <- new_name

    },

    get_inputs = function() {

      self$get_private()$inputs

    },

    get_outputs = function() {

      self$get_private()$outputs

    },

    get_required_inputs = function(outputs) {

      my_flow <- self$get_private()

      E <- try(eval(outputs), silent = TRUE)

      if (!inherits(E, "try-error") & is.character(E)) {

        outputs <- E

      } else {

        expr <- substitute(outputs)
        outputs <- as.character(expr)

        if (class(expr) == "call")
          outputs <- outputs[-1]

        env <- parent.frame(2)
        outputs <- outputs %>% .search_names(envir = env)

      }

      lapply(my_flow$required_inputs[outputs],
             function(s) my_flow$outputs[s] %>% unname())

    },

    get_process = function(output) {

      E <- try(eval(output), silent = TRUE)

      if (!inherits(E, "try-error")) {

        output <- E

      } else {

        expr <- substitute(output)
        output <- as.character(expr)

        if (class(expr) == "call")
          output <- output[-1]

        env <- parent.frame(2)
        output <- output %>% .search_names(envir = env)
      }

      my_flow <- self$get_private()

      return(my_flow$processes[[output]])

    },

    get_dependencies = function() {

      my_flow <- self$get_private()
      unique(unlist(my_flow$pkgs))

    },

    check_dependencies = function() {

      pkgs <- self$get_dependencies()

      res <- all(pkgs %in% row.names(installed.packages()))

      if (!res) {

        has_crayon <- requireNamespace("crayon", quietly = TRUE)

        cat("Currently, not all required packages for this flow are installed. Please install\n them before executing this flow:\n") #nocov

        missing <- pkgs[!(pkgs %in% installed.packages())] #nocov

        missing_deps <- str_flatten(missing, collapse = ", ") #nocov
        if (has_crayon) missing_deps <- str_flatten(red(missing), collapse = ", ") #nocov

        cat(missing_deps, "\n\n") #nocov

      }

      return(res)

    },

    replace = function(output, with) {

      E <- try(eval(output), silent = TRUE)

      if (!inherits(E, "try-error")) {

        output <- E

      } else {

        expr <- substitute(output)
        output <- as.character(expr)

        if (class(expr) == "call")
          output <- output[-1]

        env <- parent.frame(2)
        output <- output %>% .search_names(envir = env)

      }

      if (!(output %in% self$get_outputs())) {

        message <- paste0("No current definition for output = ", output)
        stop(message)

      }

      my_flow <- self$get_private()
      my_flow$processes[[output]] <- with

    },

    add = function(what = NULL, inputs = NULL, output = NULL, ...) {

      my_flow <- self$get_private()

      E <- try(eval(inputs), silent = TRUE)

      if (!inherits(E, "try-error") & is.character(E)) {

        inputs <- E

      } else {

        expr <- substitute(inputs)
        inputs <- as.character(expr)

        if (class(expr) == "call")
          inputs <- inputs[-1]

        env <- parent.frame(2)
        inputs <- inputs %>% .search_names(envir = env)

      }


      E <- try(eval(output), silent = TRUE)

      if (!inherits(E, "try-error") & is.character(E)) {

        output <- E

      } else {

        expr <- substitute(output)
        output <- as.character(expr)

        if (class(expr) == "call")
          output <- output[-1]

        env <- parent.frame(2)
        output <- output %>% .search_names(envir = env)

      }

      if (length(output) == 0L) {

        output <- NULL

      }

      # Add an input
      if (is.null(what) & is.null(output)) {

        if (is.null(inputs)) {

          stop("At least inputs must be specified.")

        }

        my_flow %>% .add_inputs(inputs = inputs)

        return(invisible(self))

      }

      # Add a DLmodel or a function
      if (inherits(what, "function")) {

        if (is.null(output)) {

          stop("An output must be provided to add a DLmodel or a function.")

        }

        if (is.null(inputs)) {

          my_flow %>% .add_process(proc = what, output = output, ...)

        } else {

          my_flow %>% .add_process(proc = what, inputs = inputs, output = output, ...)

        }


      }

    },

    execute = function(inputs = list(),
                       desired_outputs = NULL,
                       initialize_outputs = TRUE,
                       ...) {

      expr <- substitute(desired_outputs)
      desired_outputs <- as.character(expr)

      if (class(expr) == "call")
        desired_outputs <- desired_outputs[-1]

      env <- parent.frame(2)
      desired_outputs <- desired_outputs %>% .search_names(envir = env)

      my_flow <- self$get_private()
      my_flow %>% .execute_flow(inputs = inputs,
                                desired_outputs = desired_outputs,
                                initialize_outputs = initialize_outputs,
                                ...)

    },

    run = function(...) {

      self$execute(...)

    },

    compute = function(what = NULL,
                       from = list(),
                       ...) {

      expr <- substitute(what)
      what <- as.character(expr)

      if (class(expr) == "call")
        what <- what[-1]

      env <- parent.frame(2)
      what <- what %>% .search_names(envir = env)

      my_flow <- self$get_private()
      my_flow %>% .execute_flow(inputs = from,
                                desired_outputs = what,
                                initialize_outputs = TRUE,
                                ...)

    },

    log = function(level = c("DEBUG", "INFO", "WARNING", "ERROR"),
                   message = "...") {

      line_to_add <- paste0("(", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ") [",
                            level[1], "] ",
                            message)

      private$log_lines <- c(private$log_lines, line_to_add)

    },

    print_log = function(level = c("DEBUG", "WARNING", "INFO", "ERROR")) {

      all_lines <- private$log_lines

      lines <- c()

      if ("DEBUG" %in% level)
        level <- c("DEBUG", "INFO", "WARNING", "ERROR")

      for (i in seq_along(level)) {

        lines <- c(lines, grep(all_lines, pattern = level[i]))

      }

      lines <- sort(lines)

      lines <- private$log_lines[lines]

      cat(lines, sep = "\n")

    },

    errors = function() {

      self$print_log(level = "ERROR")

    },

    warnings = function() {

      self$print_log(level = "WARNING")

    },

    save_log = function(filename, level = c("DEBUG", "WARNING", "INFO", "ERROR")) {

      all_lines <- private$log_lines

      lines <- c()

      if ("DEBUG" %in% level)
        level <- c("DEBUG", "INFO", "WARNING", "ERROR")

      for (i in seq_along(level)) {

        lines <- c(lines, grep(all_lines, pattern = level[i]))

      }

      lines <- sort(lines)

      lines <- private$log_lines[lines]

      cat(lines, sep = "\n", file = filename)

    },

    graph = function() {

      my_flow <- self$get_private()
      return(my_flow$graph)

    },

    plot = function(to_file = "") {

      my_flow <- self$get_private()
      my_flow %>% .plot_flow(to_file)

    },

    memory_used = function() {

      object_size(private) + object_size(self)

    },

    save = function(path = tempdir(), file_prefix = self$name()) {

      my_flow <- self$get_private()
      my_flow %>% .save_flow(path = path, file_prefix = file_prefix)

    },

    load = function(filename) {

      self$.__enclos_env__$private <- .load_flow(filename)

    },

    deep_clone = function() {

      self$get_private() %>% .clone_flow()

    },

    subset = function(outputs) {

      expr <- substitute(outputs)
      outputs <- as.character(expr)

      if (class(expr) == "call")
        outputs <- outputs[-1]

      my_flow <- self$get_private()
      new_flow_env <- my_flow %>% .subset_flow(outputs = outputs)

      new_flow <- NIflow$new(name = self$name())
      new_flow$.__enclos_env__$private <- new_flow_env

      return(new_flow)

    },

    to_package = function(path, package_name = self$name()) {

      empty_env <- new.env()

      my_outputs <- self$get_outputs()
      my_inputs <- self$get_inputs()
      my_flow <- self$get_private()

      # Add specific functions to compute outputs
      for (output in setdiff(my_outputs, my_inputs)) {

        required_inputs <- unlist(my_flow$outputs[my_flow$required_inputs[[output]]])
        f <- .build_compute_function(args = required_inputs,
                                     output = output)

        eval(expr = parse(text = f), envir = empty_env)

      }

      # Create package
      package.skeleton(name = package_name,
                       path = path,
                       environment = empty_env)

      pkg <- file.path(path, package_name)

      # Remove "Read-and-delete-me" file
      unlink(x = file.path(pkg, "Read-and-delete-me"), force = TRUE)

      # Add dependencies to the DESCRIPTION file
      deps <- self$get_dependencies()

      invisible(sapply(deps, devtools::use_package, pkg = pkg))

      # Save the flow
      data_folder <- file.path(pkg, "inst", "flow")

      self$save(path = data_folder, file_prefix = package_name)

      # Add zzz.R which loads the flow as "flow"
      zzz_file <- file.path(pkg, "R", "zzz.R")

      string <- paste0("flow_file <- system.file('flow', '",
                       package_name , "_flow.zip', package = '",
                       package_name, "')\n\n",
                       "flow <- wf4ni::load_flow(flow_file)\n")

      cat(string, file = zzz_file)

    }

  ),

  lock_objects = FALSE

)
