#' @title Estimate of gamma by Taylor approximation
#' 
#' @description
#' Computes an initial estimate of \eqn{\gamma} based on the Taylor
#' approximation of the skewness of Lambert W \eqn{\times} Gaussian RVs around
#' \eqn{\gamma = 0}. See Details for the formula.
#' 
#' This is the initial estimate for \code{\link{IGMM}} and \code{\link{gamma_GMM}}.
#' 
#' @details
#' The second order Taylor approximation of the theoretical skewness of
#' a Lambert W x Gaussian random variable around \eqn{\gamma = 0} equals
#' \deqn{ \gamma_1(\gamma) = 6 \gamma + \mathcal{O}(\gamma^3). }
#' 
#' Ignoring higher order terms, using the empirical estimate on the left hand side,
#' and solving \eqn{\gamma} yields
#' \deqn{
#' \widehat{\gamma}_{Taylor} = \frac{1}{6} \widehat{\gamma}_1(\mathbf{y}), 
#' }
#' where \eqn{\widehat{\gamma}_1(\mathbf{y})} is the empirical skewness of the
#' data \eqn{\mathbf{y}}.
#' 
#' As the Taylor approximation is only good in a neighborhood of \eqn{\gamma =
#' 0}, the output of \code{gamma_Taylor} is restricted to the interval
#' \eqn{(-0.5, 0.5)}.
#' @param y a numeric vector of data values.
#' @param skewness.y skewness of \eqn{y}; default: empirical skewness of data
#' \code{y}.
#' @param skewness.x skewness for input X; default: 0 (symmetric input).
#' @return 
#' Scalar; the estimate of \eqn{\gamma}.
#' 
#' @seealso \code{\link{IGMM}} to estimate all parameters jointly.
#' @keywords optimize
#' @export
#' @examples
#' 
#' set.seed(2)
#' # a little skewness
#' yy <- rLambertW(n = 1000, theta = list(beta = c(0, 1), gamma = 0.1), 
#'                 distname = "normal") 
#' # Taylor estimate is good because true gamma = 0.1 close to 0
#' gamma_Taylor(yy) 
#' 
#' # very highly negatively skewed
#' yy <- rLambertW(n = 1000, theta = list(beta = c(0, 1), gamma = -0.75), 
#'                 distname = "normal") 
#' # Taylor estimate is bad since gamma = -0.75 is far from 0; 
#' # and gamma = -0.5 is the lower bound by default.
#' gamma_Taylor(yy) 
#' 
gamma_Taylor <- function(y, skewness.y = skewness(y), skewness.x = 0){
  stopifnot(is.numeric(skewness.x),
            is.numeric(skewness.y),
            length(skewness.y) == 1,
            length(skewness.x) == 1)
  gamma.hat <- (skewness.y - skewness.x) / 6
  
  if (skewness.x <= 0) {
    mu.tmp <- mean.default(y)
  } else {
    mu.tmp <- 0
  }
  bounds <- get_gamma_bounds(y, tau = c("mu_x" = mu.tmp, "sigma_x" = sd(y), 
                                        gamma = 0))
  return(sign(gamma.hat) * min(abs(gamma.hat), min(abs(bounds))))
}
