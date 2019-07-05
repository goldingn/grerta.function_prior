# S3 class stuff for the funprior class
funprior_object <- function (parameters, tf_density, target, code) {
  
  object <- list(
    parameters = parameters,
    tf_density = tf_density,
    target = target,
    code = code
  )
  
  object <- as.funprior(object)
  object

}

as.funprior <- function(x, ...) {
  class(x) <- c("funprior", class(x))
  x
}

is.funprior <- function(x, ...) {
  inherits(x, "funprior")
}

print.funprior <- function(x, ...) {
  
  top_text <- paste("a funprior object for parameters:",
                    paste(names(x$parameters), collapse = ", "))
  bottom_text <- paste(as.character(x$code)[-1], collapse = "\n")
  
  text <- paste(c(top_text, "\nwith code:\n", bottom_text), collapse = "\n")
  cat(text)
  
}

# can add a plot method for visualising the priors