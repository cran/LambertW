#' @title Iterative Generalized Method of Moments -- IGMM
#' @name IGMM

#' @description
#' An iterative method of moments estimator to find this \eqn{\tau = (\mu_x,
#' \sigma_x, \gamma)} (\eqn{\tau = (\mu_x, \sigma_x, \delta)} for \code{type = 'h'} or 
#' \eqn{\tau = (\mu_x, \sigma_x, \delta_l, \delta_r)} for \code{type = "hh"}) which
#' minimizes the distance between the sample and theoretical skewness (or
#' kurtosis) of \eqn{\boldsymbol x} and X.
#' 
#' For details of the Algorithm see the References.
#' 
#' @inheritParams common-arguments
#' @param y a numeric vector of real values.
#' @param skewness.x theoretical skewness of input X; default \code{0} (symmetric distribution).
#' @param kurtosis.x theoretical kurtosis of input X; default \code{3}
#' (Normal distribution reference).
#' @param tau.init starting values for IGMM algorithm; default: \code{\link{get_initial_tau}}.
#' See also \code{\link{gamma_Taylor}} and \code{\link{delta_Taylor}}.
#' @param robust logical; only used for \code{type = "s"}. If \code{TRUE} a 
#' robust estimate of asymmetry is used (see \code{\link{mc}}); default: \code{FALSE}.
#' @param tol a positive scalar giving the tolerance at which the distance is
#' considered close enough to zero to terminate the algorithm; default:
#' \code{.Machine$double.eps^0.25}
#' @param location.family logical; tell the algorithm whether the
#' underlying input should have a location family distribution (for example, Gaussian input);
#' default: \code{TRUE}. If \code{FALSE} (e.g., for \code{"exp"}onential input),
#'  then \code{tau['mu_x'] = 0} throughout the optimization.
#' @param not.negative logical; if \code{TRUE}, the estimate for \eqn{\gamma} or \eqn{\delta}
#' is restricted to non-negative reals. If it is set to \code{NULL}
#' (default) then it will be set internally to \code{TRUE} for heavy-tail(s)
#' Lambert W\eqn{ \times} F distributions (\code{type = "h"} or \code{"hh"}). 
#' For skewed Lambert W\eqn{ \times} F (\code{type = "s"}) it will be set to \code{FALSE}, unless it
#' is not a location-scale family (see \code{\link{get_distname_family}}).
#' @param max.iter maximum number of iterations; default: \code{100}.
#' @param delta.lower,delta.upper lower and upper bound for \code{\link{delta_GMM}} optimization.
#' By default: \code{-1} and \code{3} which covers most real-world heavy-tail scenarios.
#' 
#' @seealso
#' \code{\link{delta_GMM}}, \code{\link{gamma_GMM}}
#' @return 
#' A list of class \code{LambertW_fit}: 
#' \item{tol}{see Arguments} 
#' \item{data}{ data \code{y}}
#' \item{n}{ number of observations} 
#' \item{type}{see Arguments} 
#' \item{tau.init}{ starting values for \eqn{\tau} } 
#' \item{tau}{ IGMM estimate for \eqn{\tau} }
#' \item{tau.trace}{entire iteration trace of \eqn{\tau^{(k)}}, \eqn{k = 0, ..., K}, where 
#' \code{K <= max.iter}.}
#' \item{sub.iterations}{number of iterations only performed in GMM algorithm to find optimal \eqn{\gamma} (or \eqn{\delta})} 
#' \item{iterations}{number of iterations to update \eqn{\mu_x} and
#' \eqn{\sigma_x}. See References for detals.} 
#' \item{theta}{a list containing \eqn{\gamma} or \eqn{\delta} of IGMM.} 
#' \item{hessian}{ Hessian matrix (obtained from simulations; see References)} 
#' \item{call}{function call}
#' \item{distname}{ a character string describing distribution characteristics given
#' the target theoretical skewness/kurtosis for the input. Same information as \code{skewness.x}/\code{kurtosis.x}.} 
#' \item{skewness.x, kurtosis.x}{ see Arguments} 
#' \item{location.family}{see Arguments} 
#' \item{message}{message from the optimization method. What kind of convergence?} 
#' \item{method}{estimation method; here: \code{"IGMM"}}
#' @author Georg M. Goerg
#' @references 
#' Goerg, G.M. (2011). \dQuote{Lambert W Random Variables - A New
#' Family of Generalized Skewed Distributions with Applications to Risk
#' Estimation}. Annals of Applied Statistics, 5 (3), 2197-2230.
#' (\url{http://arxiv.org/abs/0912.4554}).
#' @keywords iteration optimize
#' @export
#' @examples
#' 
#' # estimate tau for the skewed version of a Normal
#' y <- rLambertW(n = 1000, theta = list(beta = c(2, 1), gamma = 0.2), 
#'                distname = "normal")
#' fity <- IGMM(y, type = "s")
#' fity
#' summary(fity)
#' plot(fity)
#' 
#' # estimate tau for the skewed version of an exponential
#' y <- rLambertW(n = 1000, theta = list(beta = 1, gamma = 0.5), 
#'                distname = "exp")
#' fity <- IGMM(y, type = "s", skewness.x = 2, location.family = FALSE)
#' fity
#' summary(fity)
#' plot(fity)
#' 
#' # estimate theta for the heavy-tailed version of a Normal = Tukey's h
#' y <- rLambertW(n = 500, theta = list(beta = c(2, 1), delta = 0.2), 
#'                distname = "normal")
#' system.time(
#' fity <- IGMM(y, type = "h")
#' )
#' fity
#' summary(fity)
#' plot(fity)
#' 

IGMM <- function(y, type = c("h", "hh", "s"), skewness.x = 0, kurtosis.x = 3,
                 tau.init = get_initial_tau(y, type),
                 robust = FALSE, tol = .Machine$double.eps^0.25, 
                 location.family = TRUE, not.negative = NULL,
                 max.iter = 100, delta.lower = -1, delta.upper = 3) {
  stopifnot(tol > 0, 
            is.numeric(y),
            !any(is.na(y)))
  check_tau(tau.init)
  type <- match.arg(type)
  
  if (is.null(not.negative)) {
    if (type %in% c("h", "hh")) {
      not.negative <- TRUE
    } else if (type == "s") {
      not.negative <- FALSE
    }
    if (!location.family) {
      not.negative <- TRUE
    }
  }
  
  out <- list(call = match.call(),
              tol = tol,
              data = y,
              n = length(y),
              type = type,
              not.negative = not.negative)
  if (any(type == c("h", "hh"))) {
    tau.init["sigma_x"] <- 
      tau.init["sigma_x"] / mLambertW(beta = c(0, 1), 
                                      delta = mean(tau.init[grepl("delta", names(tau.init))]), 
                                      distname = "normal")$sd
  } else if (type == "s") {
    tau.init["sigma_x"] <- 
      tau.init["sigma_x"] / mLambertW(beta = c(0, 1), gamma = tau.init["gamma"], 
                                      distname = "normal")$sd
  }
  
  if (!location.family) {
    tau.init["mu_x"] <- 0  # if it is a scale Lambert W RV only (not centered)
  }
  
  # initialize iterations
  kk <- 1  # iterations for IGMM
  total.iter <- 0  # total iterations (IGMM times gamma_GMM (or delta_GMM) in each iteration)
  # for skewed version
  if (type == "s") {
    tau.trace <- rbind(0, 
                     tau.init[c("mu_x", "sigma_x", "gamma")])
    colnames(tau.trace) <- c("mu_x", "sigma_x", "gamma")
    while (lp_norm(tau.trace[kk + 1, ] - tau.trace[kk, ], p = 2) > tol && kk < max.iter) {
      mu.hat <- tau.trace[kk + 1, "mu_x"]
      sigma.hat <- tau.trace[kk + 1, "sigma_x"]
      zz <- (y - mu.hat) / sigma.hat 
      
      DEL <- gamma_GMM(zz, gamma.init = tau.trace[kk + 1, "gamma"], skewness.x = skewness.x, 
                       robust = robust, tol = tol, not.negative = not.negative,
                       optim.fct = "nlminb")
      gamma.hat <- DEL$gamma
      
      uu <- W_gamma(zz, gamma.hat)
      xx <- uu * sigma.hat + mu.hat
      tau.trace <- rbind(tau.trace, 
                       c(mean(xx), sd(xx), gamma.hat))
      if (!location.family) {
        tau.trace[nrow(tau.trace), "mu_x"] <- 0  # for example for exponential input
      }
      total.iter <- total.iter + DEL$iterations
      kk <- kk + 1
    }
    # update last iteration with new gamma
    gamma.hat <- gamma_GMM((y - tau.trace[nrow(tau.trace), "mu_x"]) / tau.trace[nrow(tau.trace), "sigma_x"], 
                           gamma.init = tau.trace[kk + 1, "gamma"], 
                           skewness.x = skewness.x, 
                           robust = robust, tol = tol, not.negative = not.negative)$gamma
    tau.trace <- rbind(tau.trace, 
                     c(tail(tau.trace[, c("mu_x", "sigma_x")], 1), gamma.hat))
    se <- c(1, sqrt(1/2), 0.4) / sqrt(out$n)
  } else  if (type == "h") {
    # for heavy-tail versions
    tau.trace <- rbind(0, tau.init[c("mu_x", "sigma_x", "delta")])
    colnames(tau.trace) <- c("mu_x", "sigma_x", "delta")
    while (lp_norm(tau.trace[kk + 1, ] - tau.trace[kk, ], p = 2) > tol && kk < max.iter) {
      mu.hat <- tau.trace[kk + 1, "mu_x"]
      sigma.hat <- tau.trace[kk + 1, "sigma_x"]
      zz <- (y - mu.hat) / sigma.hat
      
      DEL <- delta_GMM(zz, delta.init = tau.trace[kk + 1, "delta"], 
                       kurtosis.x = kurtosis.x, tol = tol, not.negative = not.negative, 
                       type = "h")
      delta.hat <- DEL$delta
      
      uu <- W_delta(zz, delta.hat)
      xx <- uu * sigma.hat + mu.hat
      tau.trace <- rbind(tau.trace, c(mean(xx), sd(xx), delta.hat))
      if (!location.family) {
        tau.trace[nrow(tau.trace), "mu_x"] <- 0 
      }
      total.iter <- total.iter + DEL$iterations
      kk <- kk + 1
    }
    
    se <- c(1, sqrt(1/2), 1) / sqrt(length(y))
    # update delta hat
    delta.hat <- delta_GMM((y - tau.trace[nrow(tau.trace), 1])/tau.trace[nrow(tau.trace), 2], 
                           delta.init = tau.trace[nrow(tau.trace), 3], 
                           kurtosis.x = kurtosis.x, tol = tol, 
                           not.negative = not.negative, type = "h", 
                           lower = delta.lower, upper = delta.upper)$delta
    tau.trace <- rbind(tau.trace, 
                     c(tail(tau.trace[, c("mu_x", "sigma_x")], 1), delta.hat))
  } else if (type == "hh") {
    # for double heavy tail versions
    tau.trace <- rbind(0, tau.init[c("mu_x", "sigma_x", "delta_l", "delta_r")])
    colnames(tau.trace) <- c("mu_x", "sigma_x", "delta_l", "delta_r")
    while (lp_norm(tau.trace[kk + 1, ] - tau.trace[kk, ], p = 2) > tol && kk < max.iter) {
      mu.hat <- tau.trace[kk + 1, "mu_x"]
      sigma.hat <- tau.trace[kk + 1, "sigma_x"]                       
      zz <- (y - mu.hat)/sigma.hat
      
      DEL <- delta_GMM(zz, delta.init = tau.trace[kk + 1, 3:4], kurtosis.x = kurtosis.x,
                       tol = tol, not.negative = not.negative, type = "hh",
                       lower = delta.lower, upper = delta.upper)
      delta.hat <- DEL$delta
      
      uu <- W_2delta(zz, delta.hat)
      xx <- uu * sigma.hat + mu.hat
      tau.trace <- rbind(tau.trace, 
                       c(mean(xx), sd(xx), delta.hat))
      if (!location.family) {
        tau.trace[nrow(tau.trace), "mu_x"] <- 0
      }
      total.iter <- total.iter + DEL$iterations
      kk <- kk + 1
    }
    se <- c(1, sqrt(1/2), 1, 1) / sqrt(out$n)
    zz.tmp <- (y - tau.trace[nrow(tau.trace), "mu_x"])/tau.trace[nrow(tau.trace), "sigma_x"]
    delta.hat <- delta_GMM(zz.tmp, 
                           delta.init = tail(tau.trace[, c("delta_l", "delta_r")], 1), 
                           kurtosis.x = kurtosis.x, skewness.x = skewness.x, 
                           tol = tol, not.negative = not.negative, type = "hh")$delta
    tau.trace <- rbind(tau.trace, 
                     c(tail(tau.trace[, c("mu_x", "sigma_x")], 1), delta.hat))
  }
  
  tau.trace <- tau.trace[-1, ] # remove the first row (which has all 0s)
  rownames(tau.trace) <- paste("Iteration", seq_len(nrow(tau.trace)) - 1)
  
  # initial tau is the first row of tau.trace
  tau.init <- head(tau.trace, 1)[1, ]
  # estimated tau is the last iteration
  tau.est <- tail(tau.trace, 1)[1,]
  out <- c(out,
           list(tau.init = tau.init,
                tau = tau.est, 
                tau.trace = tau.trace,
                sub.iterations = total.iter,
                iterations = kk,
                hessian = -diag(1/se^2),
                skewness.x = skewness.x,
                kurtosis.x = kurtosis.x,
                location.family = location.family,
                message = paste("Conversion reached after", kk, "steps."),
                method = "IGMM"))
  if (type == "s") {
    out$distname <- paste0("Any distribution with skewness = ", skewness.x, ".")
  } else if (type == "h") {
    out$distname <- paste0("Any distribution with kurtosis = ", kurtosis.x, ".")
  }
  class(out) <- "LambertW_fit"
  return(out)
}