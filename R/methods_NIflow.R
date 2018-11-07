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

#' @title Models from a Flow
#'
#' @description This function returns a model that computes a specific output in a flow.
#'
#' @param output    (character) The name of the output that the required model computes.
#'
#' @return A \code{\link{DLmodel}} that is used to compute the given output.
#'
#' @name NIflow.get_model
NULL

#' @title Replace Part of a Flow
#'
#' @description This function is used to replace a part of the flow, such as a \code{\link{DLmodel}}, a
#' \code{\link{DLscheme}} or a function, with another object of one these classes.
#'
#' @param output    (character) Name of the output (part of the flow) to substitute.
#' @param with      (\code{\link{DLmodel}}, \code{\link{DLscheme}} or function) The object that is replacing the previous one.
#'
#' @return The flow with the replaced object.
#'
#' @name NIflow.replace
NULL

#' @title Add an Item to a Flow
#'
#' @description This function adds an item (input, \code{\link{DLscheme}}, \code{\link{DLmodel}} or function) to the flow.
#'
#' @param what      (character, \code{\link{DLscheme}}, \code{\link{DLmodel}} or function) Item to add, Default: NULL
#' @param inputs    (character) List of required inputs, Default: NULL
#' @param output    (character) The "name " of the item inside the flow, Default: NULL
#' @param subset    (list) PARAM_DESCRIPTION, Default: NULL
#'
#' @return The updated flow.
#'
#' @details If the item to add is an input, the only required (and used) argument is \code{what}. If it's a function or a \code{\link{DLscheme}} or a \code{\link{DLmodel}}, mandatory arguments are \code{what}, \code{inputs} and \code{output}.
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
#' @param mode                  (character) Inference mode, Default: c("debug", "faster", "medium", "slower")
#'
#' @return A list with as many (named) fields as desired outputs. The names of the fields are those of the outputs.
#'
#' @name NIflow.execute
NULL

#' @title Execute a Flow
#' @description This function runs the computation graph of the flow to obtain some outputs, given input files.
#' @describeIn NIflow.execute
#' @name NIflow.run
NULL

#' @title Train Part of a Flow
#'
#' @description This function is used to train a \code{\link{DLmodel}} that specifies an output of the flow.
#'
#' @param output                     (charcater) Name of the output (model) to train.
#' @param input_filenames            (list) List of input files to train with, that is, the X in the training phase.
#' @param output_filenames           (list) List of output files, representing the Y in the training phase.
#' @param train_split                (numeric) Ratio of input files used for training, the remaining are used for validation, Default: 0.75
#' @param epochs                     (numeric) Number of epochs to train for, Default: 10
#' @param target_windows_per_file    (numeric) Number of windows to extract, as a minimum, per file, Default: 1024
#' @param mode                       (character) Inference mode for the required inputs of this part of the flow, in case they had to be computed, Default: c("debug", "faster", "medium", "slower")
#'
#' @return The flow with the trained model.
#'
#' @details In case the part of the flow is currently defined by a \code{\link{DLscheme}}, a \code{\link{DLmodel}} is instatiated
#' taking into account given inputs and output filenames. This is the model to be trained.
#'
#' @name NIflow.train
NULL

#' @title Flow Graph
#'
#' @description This function returns the computation graph of the flow.
#'
#'
#' @return The graph, as an \code{\link{igraph}} object.
#'
#' @name NIflow.graph
NULL

#' @title Plot a Flow
#'
#' @description This function plots the computation graph of the flow.
#'
#' @param interactive    (logical) Use interactive JavaScript visualization?, Default: FALSE
#'
#' @name NIflow.plot
NULL

#' @title Reset Outputs of a Flow
#'
#' @description This function deletes previous computed results in a flow.
#'
#' @param outputs    (character) List of computed outputs to remove, Default: 'all'
#'
#' @name NIflow.reset
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

#' @title Clone Flow
#'
#' @description This function clones a flow.
#'
#' @param deep    (logical) Deep copy of the flow?, Default: FALSE
#'
#' @return A new flow with exactly the same information.
#'
#' @name NIflow.clone
NULL
