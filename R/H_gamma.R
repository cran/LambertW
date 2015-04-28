#' @title H transformation with gamma
#' 
#' @description
#' Skewed Lambert W\eqn{\times} F RV transformation: \eqn{H_{\gamma}(u) = u \exp(\gamma u)}.
#' 
#' @param u a numeric vector of real values.
#' @param gamma skewness parameter; default \code{gamma = 0}, which implies
#' \code{H_gamma(u) = u}.
#' @seealso
#' \code{\link{H}}
#' @return 
#' numeric; same dimension/size as \code{u}
#' @keywords math
#' @export
#' @examples
#' 
#' H_gamma(1, gamma = 1)
#' 
#' H_gamma(2) # default: gamma = 0; hence, H(u) = u
#' H_gamma(0, gamma = 1) # H_gamma(0) = 0 for all gamma
#' 
H_gamma <- function(u, gamma = 0) {
  
  stopifnot(is.numeric(gamma),
            is.numeric(u),
            length(gamma) == 1)
  
  if (gamma == 0) {
    return(u)
  } else {
    # faster and more stable than calling 1/gamma * H(gamma * u)
    return(u * exp(gamma * u))
  }
}
