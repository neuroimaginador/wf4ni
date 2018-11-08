#' @title Flow Creation
#'
#' @description This function creates a new \code{NIflow}, with given name and inputs.
#'
#' @param name      (character) Name of the flow, Default: ''
#' @param inputs    (list) Character vector or list with the names of the inputs, Default: list()
#'
#' @return A new flow.
#'
#' @name NIflow.initialize
NULL

#' @title Name of a Flow
#'
#' @description This function returns the name of a flow.
#'
#' @return The name of the flow as a string.
#'
#' @name NIflow.name
NULL

#' @title Private Environment of the Flow
#'
#' @description This function returns the private environment of the flow
#'
#' @return An environment with the flow internals
#'
#' @name NIflow.get_private
NULL

#' @title Set Name of Flow
#'
#' @description This function is used to change the name of a flow
#'
#' @param new_name    (character) New name for the flow
#'
#' @name NIflow.set_name
NULL

#' @title Flow Inputs
#'
#' @description This function returns the list of names of the inputs to this flow.
#'
#'
#' @return A character vector with the names of the inputs of the flow.
#'
#' @name NIflow.get_inputs
NULL

#' @title Flow Outputs
#'
#' @description This function returns the list of names of the outputs available in this flow.
#'
#' @return A character vector with the names of the available outputs.
#'
#' @name NIflow.get_outputs
NULL

#' @title Required Inputs
#'
#' @description This function is used to retrieve the necessary inputs that must be provided in order to compute a specific output.
#'
#' @param outputs    (list of names) Outputs for which to retrieve the mandatory inputs.
#'
#' @return A list with one field for each given output, with the name of the required inputs
#'
#' @name NIflow.get_required_inputs
NULL

#' @title Processes from a Flow
#'
#' @description This function returns a function that computes a specific output in a flow.
#'
#' @param output    (character) The name of the output that the function computes.
#'
#' @return A function that is used to compute the given output.
#'
#' @name NIflow.get_process
NULL

#' @title Dependencies of a Flow
#'
#' @description This function provides the list of packages that are needed in order to execute a flow.#'
#'
#' @return A list with the names of the needed packages.
#'
#' @name NIflow.get_dependencies
NULL

#' @title Check Dependencies
#'
#' @description This function checks that all the mandatory packages needed to execute the flow are installed locally.
#'
#' @return A logical indicating if all packages are installed or not.
#'
#' @name NIflow.check_dependencies
NULL

#' @title Replace Part of a Flow
#'
#' @description This function is used to replace a part of the flow, such as a function, with another object of one this class.
#'
#' @param output    (character) Name of the output (part of the flow) to substitute.
#' @param with      (function) The object that is replacing the previous one.
#'
#' @return The flow with the replaced object.
#'
#' @name NIflow.replace
NULL

#' @title Add an Item to a Flow
#'
#' @description This function adds an item (an input or a function) to the flow.
#'
#' @param what      (character, or function) Item to add, Default: NULL
#' @param inputs    (character) List of required inputs, Default: NULL
#' @param output    (character) The "name" of the item inside the flow, Default: NULL
#'
#' @return The updated flow.
#'
#' @details If the item to add is an input, the only required (and used) argument is \code{what}. If it's a function, mandatory arguments are \code{what}, \code{inputs} and \code{output}.
#'
#' @name NIflow.add
NULL

#' @title Execute a Flow
#'
#' @description This function runs the computation graph of the flow to obtain some outputs, given input files.
#'
#' @param inputs                (list) List of input filenames, Default: list()
#' @param desired_outputs       (character vector) List of names of the outputs to compute, Default: NULL
#' @param initialize_outputs    (logical) Delete previous runs?, Default: TRUE
#'
#' @return A list with as many (named) fields as desired outputs. The names of the fields are those of the outputs.
#'
#' @name NIflow.execute
NULL

#' @title Execute a Flow
#'
#' @description This function runs the computation graph of the flow to obtain some outputs, given input files.
#'
#' @param inputs                (list) List of input filenames, Default: list()
#' @param desired_outputs       (character vector) List of names of the outputs to compute, Default: NULL
#' @param initialize_outputs    (logical) Delete previous runs?, Default: TRUE
#'
#' @return A list with as many (named) fields as desired outputs. The names of the fields are those of the outputs.
#'
#' @name NIflow.run
NULL

#' @title Log a Message Interally
#'
#' @description This function is used internally by the flow to log some events.
#'
#' @param level      (character) Level for the message, which indicates its importance, being the most trivial "DEBUG" and the most important "ERROR", Default: c("DEBUG", "INFO", "WARNING", "ERROR")
#' @param message    (character) The message to log, Default: '...'
#'
#' @name NIflow.log
NULL

#' @title Print Log
#'
#' @description With this function, the internal log of the flow is printed.
#'
#' @param level    (character) The level for which to print related messages. DEBUG prints all information, Default: c("DEBUG", "WARNING", "INFO", "ERROR")
#'
#' @return Prints in the console the required internal log.
#'
#' @name NIflow.print_log
NULL

#' @title Print Errors
#'
#' @description This function prints in the console just the errors contained in the log.
#'
#' @name NIflow.errors
NULL

#' @title Print Warnings
#'
#' @description This function prints in the console just the warnings contained in the log.
#'
#' @name NIflow.warnings
NULL

#' @title Save Log
#'
#' @description This function is used to save the internal log of the flow.
#'
#' @param filename    (character) Path to the file where to save the text log.
#' @param level       (character) Which levels to save to disk? DEBUG stands for all, Default: c("DEBUG", "WARNING", "INFO", "ERROR")
#'
#'
#' @name NIflow.save_log
NULL

#' @title Flow Graph
#'
#' @description This function returns the computation graph of the flow.
#'
#' @return The graph, as an \code{\link{igraph}} object.
#'
#' @name NIflow.graph
NULL

#' @title Plot a Flow
#'
#' @description This function plots the computation graph of the flow.
#'
#' @name NIflow.plot
NULL

#' @title Memory Used
#'
#' @description This function computes the amount of memory used by the flow, including its internals.
#'
#' @return The memory size of the flow.
#'
#' @seealso
#'  \code{\link[pryr]{object_size}}
#' @name NIflow.memory_used
NULL

#' @title Save a Flow
#'
#' @description This function saves a flow to disk.
#'
#' @param path           (character) Path where to store the flow, Default: tempdir()
#' @param file_prefix    (character) Filename, Default: the flow name.
#'
#' @name NIflow.save
NULL

#' @title Load a Flow
#'
#' @description This function loads a flow from disk.
#'
#' @param filename    (character) Path to a file where a flow is stored.
#'
#' @return The loaded flow.
#'
#' @name NIflow.load
NULL

#' @title Clone Flow
#'
#' @description This function clones a flow.
#'
#' @return A new flow with exactly the same information.
#'
#' @name NIflow.deep_clone
NULL

#' @title Flow Subsetting
#'
#' @description This function extracts just a subflow from the parent flow, specifying the outputs to retain.
#'
#' @param outputs    (character vector) List of the outputs to retain.
#'
#' @return A new flow which is complete in the sense that it contains the required \code{outputs} and all parts of the bigger flow
#' which lead to those outputs.
#'
#' @name NIflow.subset
NULL

#' @title Build Package from Flow
#'
#' @description With this function, an R package is created that includes the current flow and functions to compute all of its possible outputs.
#'
#' @param path            (character) Path where to create a package
#' @param package_name    (character) Name of the package to create, Default: the name of the current flow
#'
#' @seealso
#'  \code{\link[devtools]{use_package}}
#'
#' @name NIflow.to_package
NULL

