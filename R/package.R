# this R file is just here to produce a package-level helpfile for people to
# look at when they get started

#' @title Specifying Priors Over Parametric Functions in greta
#' @name greta.function_prior
#'
#' @description It's common in Bayesian statistical analyses to have prior
#'   information on summary statistics of a model rather than on the model
#'   parameters themselves. For example, you may have prior information about
#'   the probability predicted by a logistic regression under some set of
#'   covariates, or the timing of peaks in a dynamical model. In these
#'   situations it can be difficult to determine the priors on the model
#'   parameters that correspond to these priors on summary statistics.
#'   greta.function_prior lets you define priors on these summary statistics by
#'   considering them as priors over parametric functions, and provides a simple
#'   interface for fitting models.
#'
#' @docType package
#'
#' @importFrom tensorflow tf
#' @importFrom greta .internals
#'
#' @examples
#'
#' # add a simple example here to introduce the package!
#' 
NULL