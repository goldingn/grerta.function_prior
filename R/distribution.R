#' @name distribution
#' @aliases distribution
#' @title define a distribution over greta arrays
#'
#' @description \code{distribution} defines probability distributions over greta
#'   arrays, e.g. to set a model likelihood, or a prior. the \code{description}
#'   function provided by greta.funprior differs from the version in greta in
#'   that it allows you to define functions over (almost) any greta array, be it
#'   a data, variable or operation greta array. The only exceptions are greta
#'   arrays that already have a distribution (there can be only one) and greta
#'   arrays created with discrete operations (we can't compute a gradient).
#'
#' @details To enable users to set distributions on variables and operations
#'   whilst ensuring a valid model, greta.funprior computes the appropriate log
#'   Jacobian determinant adjustment by automatic differentiation and adds this
#'   to the model definition for you.
#'
#' @param greta_array a greta array. For the assignment method it must not
#'   already have a probability distribution assigned.
#'
#' @param value a greta array with a distribution (see
#'   \code{\link{distributions}})
#'
#' @details The extract method returns the greta array if it has a distribution,
#'   or \code{NULL} if it doesn't. It has no real use-case, but is included for
#'   completeness
#'
#' @export
#' @examples
#' \dontrun{
#'
#' # define a model likelihood
#'
#' # observed data and mean parameter to be estimated
#' # (explicitly coerce data to a greta array so we can refer to it later)
#' y <- as_data(rnorm(5, 0, 3))
#'
#' mu <- uniform(-3, 3)
#'
#' # define the distribution over y (the model likelihood)
#' distribution(y) <- normal(mu, 1)
#'
#' # get the distribution over y
#' distribution(y)
#' }
`distribution<-` <- function(greta_array, value) {
  
  # stash the old greta array to return
  greta_array_tmp <- greta_array
  
  # coerce to a greta array (converts numerics to data arrays)
  greta_array <- as.greta_array(greta_array)
  
  node <- get_node(greta_array)
  
  # only for greta arrays without distributions
  if (!is.null(node$distribution)) {
    stop("left hand side already has a distribution assigned",
         call. = FALSE)
  }
  
  # can only assign with greta arrays ...
  if (!inherits(value, "greta_array")) {
    stop("right hand side must be a greta array",
         call. = FALSE)
  }
  
  # ... that have distributions
  value_node <- get_node(value)
  distribution_node <- value_node$distribution
  
  if (!inherits(distribution_node, "distribution_node")) {
    stop("right hand side must have a distribution",
         call. = FALSE)
  }
  
  # that aren't already fixed
  if (inherits(distribution_node$target, "data_node")) {
    stop("right hand side has already been assigned fixed values",
         call. = FALSE)
  }
  
  # if distribution isn't scalar, make sure it has the right dimensions
  if (!is_scalar(value)) {
    if (!identical(dim(greta_array), dim(value))) {
      stop("left and right hand sides have different dimensions. ",
           "The distribution must have dimension of either ",
           paste(dim(greta_array), collapse = " x "),
           " or 1 x 1, but instead has dimension ",
           paste(dim(value), collapse = " x "),
           call. = FALSE)
    }
  }
  
  # if this is a variable, make sure its support matches that of the
  # distribution
  stop("check for matching support not yet implemented")
  
  # search for variables upstream in the dag from this node (if not, it's data
  # so no problem)
  upstream_variable_nodes <- find_upstream(node, "variable")
  
  if (length(upstream_variable_nodes) > 0) {
    
    # tell the distribution node it needs to try to compute the log jacobian
    # determinant from this node to the upstream variables
    
    # hack the density to add on the ljac here
    old_tf_log_density_function <- distribution_node$tf_log_density_function
    new_tf_log_density_function <- function (x, parameters, dag) {
      log_density <- old_tf_log_density_function(x, parameters, dag)
      ljac <- compute_ljac(x, upstream_variable_nodes, dag)
      log_density + ljac
    }
    
    # put it in the same scope, but add in the required objects
    env <- environment(new_tf_log_density_function)
    parent.env(env) <- parent.env(tf_log_density_function)
    env$old_tf_log_density_function <- old_tf_log_density_function
    env$compute_ljac <- compute_ljac
    env$upstream_variable_nodes <- upstream_variable_nodes
    
    # put it back in the distribution node
    unlockBinding("tf_log_density_function", distribution_node)
    distribution_node$tf_log_density_function <- new_tf_log_density_function
    lockBinding("tf_log_density_function", distribution_node)
    
  }
  
  # assign the new node as the distribution's target
  # also adds distribution_node as this node's distribution
  distribution_node$remove_target()
  distribution_node$add_target(node)
  
  # remove the distribution from the RHS variable greta array
  value_node$distribution <- NULL
  
  # return greta_array (pre-conversion to a greta array)
  greta_array_tmp
  
}

#' @rdname distribution
#' @export
distribution <- function(greta_array) {
  
  # only for greta arrays
  if (!inherits(greta_array, "greta_array")) {
    stop("not a greta array",
         call. = FALSE)
  }
  
  # if greta_array has a distribution, return this greta array
  if (inherits(get_node(greta_array)$distribution, "distribution_node")) {
    
    distrib <- greta_array
    
  } else {
    
    # otherwise return NULL
    distrib <- NULL
    
  }
  
  distrib
  
}

# search recursively though nodes from which this one inherits, finding all
# nodes of a certain type
find_upstream <- function(node,
                          type = c(
                            "data",
                            "operation",
                            "variable",
                            "distribution"
                          ),
                          starting_with = list()) {
  
  target_type <- match.arg(type)
  
  matching_nodes <- starting_with
  
  # check this one first
  if (greta:::node_type(node) == target_type) {
    matching_nodes[[node$unique_name]] <- node
  }
  
  # the child/parent semantics are the wrong way around because I was briefly
  # confused several years ago. children are the ones from which this was
  # created.
  for (child in c(node$children)) {
    
    this_type <- greta:::node_type(child)
    right_type <- this_type == target_type
    
    # if it's the right type and new, add it to the list
    if (right_type && !child$unique_name %in% matching_nodes) {
        matching_nodes[[child$unique_name]] <- child
    }
    
    # if it's not the right type, or it's a distribution or operation node, keep
    # recursing (data and variable nodes are terminal if the target type)
    if (!right_type | this_type %in% c("distribution", "operation")) {
      matching_nodes <- find_upstream(child, target_type, starting_with = matching_nodes)
    }

  }
  
  # return the list
  matching_nodes
  
}

# given a tensor, nodes for the upstream variables from which it was
# constructed, and the dag, find the upstream tensors and compute the log
# determinant of the jacobian from the upstream tensors to this one
compute_ljac <- function (target_tensor, upstream_nodes, dag) {
  
  # get the list of upstream tensors
  tfe <- dag$tf_environment
  upstream_tensor_names <- lapply(upstream_nodes, dag$tf_name)
  upstream_tensors <- lapply(upstream_tensor_names, function (name) tfe[[name]])
  
  stop("accounting for the log jacobian determinant is not yet implemented")
  
  # compute the log determinant of the jacobian between the upstream tensors and
  # this one, preserving the batch dimension

  # probably using tf$gradients()
  
  # if there is not a valid gradient (e.g. if an op with no gradient was used),
  # the gradients function should return a NULL for that entry. In that
  # situation, we should error with a helpful message about the log jacobian
  # adjustment not being calculable
  
  missing_gradients <- vapply(gradients, is.null, logical(1))
  if (any(missing_gradients)) {
    stop("it was not possible to compute the necessary gradient adjustment to ",
         "assign a distribution to this greta array, possibly because it was ",
         "calculated with function that has no gradient", call. = FALSE)
  }
  
}


as.greta_array <- greta::.internals$greta_arrays$as.greta_array
get_node <- greta::.internals$greta_arrays$get_node
is_scalar <- greta::.internals$utils$misc$is_scalar
