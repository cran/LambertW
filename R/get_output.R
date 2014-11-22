#' @title Transform input X to output Y
#' @name get_output
#' 
#' @description
#' \code{get_output} transforms the input \eqn{\boldsymbol x} to the observed data
#' \eqn{\boldsymbol y} given the transformation vector \eqn{\tau =
#' (\mu_x(\boldsymbol \beta), \sigma_x(\boldsymbol \beta), \gamma, \alpha, \delta)}.
#' 
#' @param x a numeric vector of data values.
#' @param return.z should the shifted and scaled output also be returned? Default: \code{FALSE}.
#' @inheritParams common-arguments
#' @return 
#' A numeric object of same size/dimension as input \code{x}.
#' 
#' If \code{return.z = TRUE}, then it returns a list with 2 vectors
#' \item{z}{shifted and scaled input \eqn{\boldsymbol z}, } 
#' \item{y}{transformed output data \eqn{\boldsymbol y}, which has a Lambert W
#' \eqn{\times} F distribution.}
#' @keywords manip
#' @seealso
#' \code{\link{get_input}}; \code{\link{Gaussianize}} with argument \code{inverse = TRUE}.
#' @export
#' @examples
#' 
#' tau.tmp <- c(mu_x = 0, sigma_x = 1, delta = 0.2)
#' xx <- rnorm(100)
#' yy <- get_output(xx, tau.tmp)
#' xx.hat <- get_input(yy, tau.tmp)
#' # they must be equal (except for numerical issues)
#' stopifnot(lp_norm(xx - xx.hat, 1) < 0.01)

get_output <- function(x, tau, return.z = FALSE) {

  stopifnot(is.numeric(x),
            !any(is.na(x)))
  
  tau <- complete_tau(tau)
  check_tau(tau)
  type.tmp <- tau2type(tau)
  
  uu <- (x - tau["mu_x"]) / tau["sigma_x"]
  if (type.tmp == "s") {
    zz <- H_gamma(uu, gamma = tau["gamma"])
  } else if (type.tmp == "h") {
    zz <- G_delta_alpha(uu, delta = tau["delta"], alpha = tau["alpha"])
  } else if (type.tmp == "hh") {
    zz <- G_2delta_2alpha(uu, delta = tau[c("delta_l", "delta_r")], alpha = tau[c("alpha_l", "alpha_r")])
  } else {
    stop("Something went wrong with the 'type' argument. Type ", type.tmp, " is not valid.")
  }
  yy <- zz * tau["sigma_x"] + tau["mu_x"]
  names(yy) <- NULL
  
  if (return.z) {
    return(list(z = zz,
                y = yy))
  } else {
    return(yy)
  }
}
