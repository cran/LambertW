#' @title Lambert W function, its logarithm and derivative
#' @name W
#' @aliases deriv_W
#' @rdname W
#' 
#' @description 
#' Evaluates the Lambert W function (\code{W}), its first derivative
#' (\code{deriv_W}), and its logarithm (\code{log_W}).  All of them have 
#' a principal and non-principal branch solution 
#' (\code{branch = 0} (default) or \code{branch = -1}).
#' 
#' \code{W} is a wrapper for \code{\link[gsl]{lambert_W0}} and
#' \code{\link[gsl]{lambert_Wm1}} in the \pkg{gsl} package.
#' 
#' @details
#' The Lambert W function \eqn{W(z) = u} is defined as the inverse
#' of (see \code{\link{H}})
#' \deqn{ u \exp(u) = z, }
#' i.e., it satisfies \eqn{W(z) \exp(W(z)) = z}.
#' 
#' Depending \eqn{z} we can distinguish 3 cases:
#' \describe{
#' \item{\eqn{z \geq 0}}{solution is unique \code{W(z) = W(z, branch = 0) = W(z, branch = -1)};}
#' \item{\eqn{-1/e \leq z < 0}}{two solutions: the principal (\code{W(z, branch = 0)})
#' and non-principal (\code{W(z, branch = -1)}) branch;}
#' \item{\eqn{z < -1/e}}{ no solution exists in the reals.}
#' }
#' 
#' \code{log_W} computes the natural logarithm of \eqn{W(z)}. 
#' This can be done efficiently since \eqn{\log W(z) = \log z - W(z)}.
#' 
#' The derivative can be expressed as a function of \eqn{W(z)}: \deqn{ W'(z) =
#' \frac{1}{(1 + W(z)) \exp(W(z))} = \frac{W(z)}{z(1 + W(z))}. }
#' 
#' Moreover, by taking logs on both sides we can even simplify further to
#' \deqn{ \log W'(z) = \log W(z) - \log z - \log (1 + W(z))}
#' which -- since \eqn{\log W(z) = \log z - W(z)} -- simplifies to
#' \deqn{ \log W'(z) = - W(z) - \log (1 + W(z)).}
#' 
#' Note that \eqn{W'(0) = 1} and \eqn{W'(-1/e) = \infty}. For this reason it is numerically 
#' faster to pass the value of \eqn{W(z)} as an argument to the derivative 
#' \code{deriv_W}, as it has often been already evaluated in a previous step.
#' 

#' 
#' @param z a numeric vector of real values; note that \code{W(Inf) = Inf}.
#' @param W.z Lambert W function evaluated at \code{z}; see Details below for why this is a useful argument.
#' @param branch either \code{0} or \code{-1} for the principal or non-principal branch solution.
#' @return 
#' numeric; same dimensions/size as \code{z}.
#' 
#' \code{W} returns numeric, \code{Inf} (for \code{z = Inf}), or \code{NA} if \eqn{z < -1/e}. 
#' 
#' Note that \code{W} handles \code{Inf} and \code{NaN} differently to
#'  \code{\link[lamW]{lambertW0_C}} and \code{\link[lamW]{lambertWm1_C}} 
#'  in the \pkg{lamW} package.  In both cases it returns \code{NA}.
#' @seealso 
#' \code{\link[lamW]{lambertW0_C}} and \code{\link[lamW]{lambertWm1_C}} in
#' the \pkg{lamW} package.
#' @importFrom lamW lambertW0_C
#' @importFrom lamW lambertWm1_C
#' @references 
#' Corless, R. M., G. H. Gonnet, D. E. G. Hare, D. J. Jeffrey and D. E. Knuth
#' (1996). \dQuote{On the Lambert W function}. Advances in Computational Mathematics, pp. 329-359.
#' @keywords math
#' @export
#' @examples
#'  
#' W(-0.25) # "reasonable" input event
#' W(-0.25, branch = -1) # "extreme" input event
#' 
#' curve(W(x, branch = -1), -1, 2, type="l", col = 2, lwd = 2)
#' curve(W(x), -1, 2, type="l", add = TRUE, lty = 2)
#' abline(v = - 1 / exp(1))
#' 
#' # For lower values, the principal branch gives the 'wrong' solution; 
#' # the non-principal must be used.
#' H(-10)
#' W(H(-10), branch = 0)
#' W(H(-10), branch = -1)

W <- function(z, branch = 0) {
  stopifnot(length(branch) == 1,
            is.numeric(z))
  if (anyNA(z)) {
    warning("Some values of ", deparse(substitute(z)), " are NA or NaN. ",
            "Returning 'NA' for these entries.")
    non.na.z <- z[!is.na(z)]
  } else {
    non.na.z <- z
  }
  W.non.na.z <- rep(NA, length(non.na.z))
  if (branch == 0) {
    W.non.na.z <- lamW::lambertW0_C(non.na.z)
  } else if (branch == -1) {
    if (any(is.infinite(z))) {
      stop("'Inf' is not a valid argument of the non-principal branch W", 
           " (branch = -1).")
    } 
    #else if (any(z > 0 || z < -exp(-1))) {
    #  warning("The non-principal branch is only defined for negative values ",
    #          "between -exp(-1) and 0.")
    #}
    W.non.na.z <- lamW::lambertWm1_C(non.na.z)
  } else {
    stop("Branch was ", branch,  "; must be either '0' or '-1'.")
  }
  W.non.na.z[is.infinite(non.na.z)] <- Inf
  
  if (length(W.non.na.z) == length(z)) {
    dim(W.non.na.z) <- dim(z)
    return(W.non.na.z)
  } else {
    W.z <- rep(NA, length(z))
    W.z[!is.na(z)] <- W.non.na.z
    # replace NaN with NA
    W.z[is.nan(W.z)] <- NA
    dim(W.z) <- dim(z)
    return(W.z)
  }
}

