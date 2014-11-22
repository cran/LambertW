#' @title Transformation that defines the Lambert W function
#' 
#' @description
#' The Lambert W function \eqn{W(z)} is the inverse of \eqn{H(u) = u \exp(u) = z}.
#' 
#' @param u a numeric vector of real/complex values.
#' @return 
#' Returns \eqn{z = u \exp(u)} for \eqn{u \in C}. If \eqn{u} is a
#' vector/matrix, so is \eqn{z}.
#' @seealso 
#' \code{\link{W}}
#' @keywords math
#' @export
#' @examples
#' 
#' H(0)
#' H(10)
#' 
#' plot(H, -5, 0.5, type="l", xlab="u", ylab="z")
#' grid()
#' abline(h=0, lty = 2)
#' abline(v=0, lty = 2)
#' 
H <- function(u) {
  stopifnot(is.numeric(u))
  u * exp(u)
}
