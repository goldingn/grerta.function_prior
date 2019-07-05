# we want to specify a prior over this *parametric* function. That is, use a a
# predetermined parametric function, but provide prior knowledge on some or all
# of the function values, rather than directly placing them on the parameters.

#' @title construct a prior over a parametric function in greta
#'
#' @param \dots greta arrays representing the parameters in your function
#' @param expr an expression (R code in curly braces) stating your prior beliefs
#'   about the function. This should use \code{distribution()} on quantities
#'   computed from your function and should no introduce any more parameters to
#'   the model.
#'
#' @export
#' @importFrom R6 R6Class
#'
#' @examples
#' # growth function
#' growth <- function (age, alpha, beta, gamma) {
#'   age2 <- (age - 5) ^ 2
#'   (alpha * age2) / (beta + age2) + gamma
#' }
#'
#' # true parameters
#' alpha_true <- 3
#' beta_true <- 5
#' gamma_true <- 170
#' sd_true <- 3
#'
#' # fake data
#' n <- 100
#' ages <- runif(n, 0, 100)
#' mean_heights_true <- growth(ages, alpha_true, beta_true, gamma_true)
#' heights <- rnorm(n, mean_heights_true, sd_true)
#'
#' # fit a model
#' library(greta)
#'
#' # growth model parameters - improper flat priors on these
#' alpha <- variable()
#' beta <- variable(lower = 0)
#' gamma <- variable(lower = 0)
#'
#' # the likelihood
#' sd <- cauchy(0, 1, truncation = c(0, Inf))
#' mean_heights <- growth(ages, alpha, beta, gamma)
#' distribution(heights) <- normal(mean_heights, sd)
#'
#' # we have prior knowledge that the population mean height at age 25 is around
#' # 171, with a standard deviation (estimation error) of 0.5. We also have prior
#' # knowledge that the maximum year-on-year growth is around 30cm, with standard
#' # deviation 5, but must be positive. We provide some code specifying this prior
#' # belief
#' fpri <- funprior(alpha, beta, gamma,
#'                  code = {
#'
#'                    time_diff <- 1
#'                    time_seq <- seq(0, 30, by = time_diff)
#'                    height_seq <- growth(time_seq, alpha, beta, gamma)
#'                    grad <- diff(height_seq) / time_diff
#'
#'                    max_grad <- max(grad)
#'                    height_25 <- growth(25, alpha, beta, gamma)
#'
#'                    distribution(max_grad) <-  normal(30, 5, truncation = c(0, Inf))
#'                    distribution(height_25) <- normal(171, 0.5)
#'
#'                  })
#'
#' # the model will have been updated with this prior knowledge, and you can
#' # inspect the object `fpri` to get intuition about the prior implied on the
#' # parameters
#'
#' m <- model(alpha, beta, gamma, sd)
funprior <- function (..., expr) {
  
  # catch the code and the parent environment
  code <- substitute(expr)
  env <- parent.frame()
  
  # get a named list of parameters (guess names from call)
  params <- list(...)
  names <- names(params)
  cl <- match.call()
  nm <- as.character(as.list(cl)[-1])
  if (is.null(names)) {
    names(params) <- nm
  } else {
    blank_names <- names == ""
    names[blank_names] <- nm[blank_names]
    names(params) <- names
  }

  # check inputs are variable greta arrays
  for (param in params) {
    
    is_greta_array <- inherits(param, "greta_array")
    
    if (is_greta_array) {
      param_node <- greta:::get_node(param)
      is_variable <- greta:::node_type(param_node) == "variable"
    } else {
      is_variable <- FALSE
    }

    if (!is_variable) {
      stop ("the arguments to funprior must be variable greta arrays ",
            "(ie. created with variable() or a probability distribution)",
            "and some code in curly braces", call. = FALSE)
    }
    
  }
  
  # mess with the dag to build the tf_density function
  # (checking no new parameters are added and check densities are defined)
  r_function <- build_r_function(names(params), code, env)
  tf_density <- get_tf_density(r_function, params)
  
  # combine the parameters into a single parameter vector, that will notionally
  # be the target of the prior distribution
  theta <- do.call(`c`, params)
  prior <- distrib(args_list = params, tf_density = tf_density, dim = dim(theta))
  distribution(theta) <- prior
  
  # invisibly return an object we can do stuff (like plotting) with later
  result <- funprior_object(parameters = params,
                 tf_density = tf_density,
                 target = theta,
                 code = code)
  
  class(result) <- c("funprior", class(result))
  
  invisible(result)
  
}

# a distribution node to represent this prior. Mostly empty, since the user won't
# actually deal with it
functional_prior_distribution <- R6Class(
  "functional_prior_distribution",
  inherit = distribution_node,
  public = list(
    
    tf_density = NULL,
    args_list = list(),
    
    initialize = function(args_list, tf_density, dim = NULL) {
      
      self$args_list <- args_list
      self$tf_density <- tf_density
      super$initialize("funprior", dim)
      
    },
    
    tf_distrib = function(parameters, dag) {
      
      nodes_list <- lapply(self$args_list, greta::get_node)
      
      # rather than the target, find the tensors from which it was created
      unpack <- function () {
        
        stop ("not implemented")
        
        # get the tensors corresponding to the arguments in args_list
        dag$get_tf_name(nodes_list)

        
        # can do this with nodes. May be possible to do it with tensors instead?
        # that would be cleaner, but tricky
      }
      
      log_prob <- function() {
        
        # get the tensors for unpacked theta and run density on that
        x_list <- unpack(x)
        do.call(self$tf_density, x_list)
        
      }
      
      list(log_prob = log_prob, cdf = NULL, log_cdf = NULL)
      
    },
    
    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL
    
  )
)

# given a vector of names of input greta arrays arguments, an R expression stating the
# prior knowledge, and the parent environment from which to collect objects,
# return an R function evaluating the code on the inputs.
build_r_function <- function(arg_names, code, env) {
  
  # create a function where the order of inputs don't matter, since we
  # can match the names from parameters to the code
  r_fun <- function() {}
  args <- replicate(length(arg_names), NULL, simplify = FALSE)
  names(args) <- arg_names
  formals(r_fun) <- args
  body(r_fun) <- code
  `parent.env<-`(r_fun) <- env
  r_fun
  
}

# given an R function stating the user's prior belief about the distribution
# over the function, and a list of greta arrays (parameters of the function) in
# the same order as the arguments, return a tensorflow function taking in the
# tensors corresponding to the parameters, and return a tensor giving the
# density, the sum of densities defined in `code`, and the log determinant of
# the Jacobian mapping from the parameters to the targets of those distributions
get_tf_density <- function(r_fun, parameters) {
  
  stop ("not implemented")
  
  # something like this, but modifying the output of the function to be the sum
  # of the densities, and the log jacobian adjustment
  
  # need to check that there are densities and that no new parameters are added
  inputs <- c(list(r_fun), parameters)
  tf_fun <- do.call(greta:::as_tf_function, inputs)
  
}


# to define pullback density function: create a session with tensors for the parameters pass in values for the parameters, and
# brind back the densities from the tf_density functions. the densities out of
# the distributions in the dag. Run the log jacobian adjustment and add that to
# the density. return that density.

# using it in a model: use the combined density as the tf_log_prob function, and
# assign that as a distribution over the combined parameters.

# find the targets of the distributions and compute the ljac from the inputs to those





