#' @rdname tau-utils
#' @description
#' \code{normalize_by_tau} shifts and scales data given the \code{tau} vector as
#' \deqn{(data - \mu_x) / \sigma_x}.
#' 
#' Note that \eqn{\mu_x} and \eqn{\sigma_x} are not necessarily mean and standard
#' deviation in the \eqn{\tau} vector; that depends on the family type (for location
#' families they usually are mean and standard deviation; for scale and non-location
#' non-scale families they are just parameters for the transformation).
#' 
#' @param data numeric; a numeric object in R.  Usually this is either 
#' \code{y} or \code{x} (or \code{z} and \code{u} if \code{inverse = TRUE}.)
#' @param inverse logical; if \code{TRUE} it applies the inverse transformation
#' \eqn{data \cdot \sigma_x + \mu_x}
#' @export

normalize_by_tau <- function(data, tau, inverse = FALSE) {
  
  stopifnot(is.numeric(data),
            "mu_x" %in% names(tau),
            "sigma_x" %in% names(tau),
            is.logical(inverse))
  
  if (inverse) {
    new.data <- data * tau["sigma_x"] + tau["mu_x"]
  } else {
    new.data <- (data - tau["mu_x"]) / tau["sigma_x"]
  }
  
  return(new.data)
} 