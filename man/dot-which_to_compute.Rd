% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/execute_flow.R
\name{.which_to_compute}
\alias{.which_to_compute}
\title{Pipeline to Execute}
\usage{
.which_to_compute(flow, output, given_inputs = NULL)
}
\arguments{
\item{flow}{(a NIflow object) The flow.}

\item{output}{(character) The name of the output to compute.}

\item{given_inputs}{(list) Additional inputs provided, Default: NULL}
}
\value{
A vector with the corresponding steps needed to calculate the \code{output}.
}
\description{
This internal function returns the list of steps to compute the given output.
}
\details{
DETAILS
}
