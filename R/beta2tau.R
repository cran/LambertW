#' @rdname beta-utils
#' @description 
#' \code{beta2tau} converts \eqn{\boldsymbol \beta} to 
#' the parameter \eqn{\tau = (\mu_x, \sigma_x, \gamma = 0, \alpha = 1, \delta = 0)},
#' which defines the Lambert W\eqn{\times} F random variable transformation from 
#' \eqn{X} to \eqn{Y} (see \code{\link{tau-utils}}). Note that mean 
#' \eqn{\mu_x} and standard deviation \eqn{\sigma_x} of \eqn{X} in general depend 
#' on \eqn{\boldsymbol \beta}.
#' @return
#' \code{beta2tau} returns a numeric vector, which is the \eqn{\tau} vector implied
#' by \code{beta} and \code{distname}.
#' @export
#' @seealso
#' \code{\link{tau-utils}}, \code{\link{theta-utils}}
#' @examples
#' # By default: delta = gamma = 0 and alpha = 1
#' beta2tau(c(1, 1), distname = "normal") 
#' \dontrun{
#' beta2tau(c(1, 4, 1), distname = "t")
#' }
#' beta2tau(c(1, 4, 3), distname = "t") # no problem
#' 
beta2tau <- function(beta, distname) {
  stopifnot(is.numeric(beta))
  check_distname(distname)
  check_beta(beta, distname = distname)
  names(beta) <- get_beta_names(distname)
  # note that sigma_x is standard deviation, not variance.
  tau <- c()
  switch(distname,
         chisq = {tau[c("mu_x", "sigma_x")] <- c(0, 2 * beta)},
         exp = {tau[c("mu_x", "sigma_x")] <- c(0, 1 / beta[1])},
         "f" = {tau["mu_x"] <- 0
              tau["sigma_x"] <- sqrt((2 * beta[2]^2 * (beta[1] + beta[2] - 2))/
                  (beta[1] * (beta[2] - 2)^2 * (beta[2] - 4)))
          },
         gamma = {
             tau["mu_x"] <- 0
             tau["sigma_x"] <- sqrt(beta["shape"]) * beta["scale"]
         },
         laplace = {
             tau["mu_x"] <- beta[1]
             tau["sigma_x"] <- sqrt(2) * beta[2]
         },
         normal = {tau[c("mu_x", "sigma_x")] <- beta[1:2]},
         t = {
             nu <- beta[3]
             if (nu <= 2) {
                 stop("A t-distribution with df = ", nu,
                      " does not have finite variance, which is required",
                      " for a location-scale Lambert W x t distribution.",
                      " Please fix.")
             }
             ss <- beta[2]
             scaling.factor <- sqrt(nu/(nu - 2))
             tau["mu_x"] <- beta[1]
             tau["sigma_x"] <- ss * scaling.factor
         },
         unif = {  
             tau["mu_x"] <- 0.5 * (beta[1] + beta[2])
             tau["sigma_x"] <- sqrt(1/12 * (beta[2] - beta[1])^2)
         }
         )
  if (length(tau) == 0) {
    stop("Seems like distribution '", distname, "' is not supported.")    
  }

  # use default values here
  tau <- c(tau, c(alpha = 1, gamma = 0, delta = 0))
  check_tau(tau)
  return(tau)
} 
