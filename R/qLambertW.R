#' @rdname LambertW-utils
#' @export
qLambertW <- function(p, distname, theta = NULL, beta = NULL, gamma = 0, delta = 0, alpha = 1, 
                      input.u = NULL, tau = NULL) {
  if (p < 0 || p > 1) {
    stop("Probability 'p' must lie in [0, 1].")
  }
  
  if (is.null(theta)) {
    theta <- list(beta = beta, alpha = alpha, gamma = gamma, delta = delta)
  } 
  theta <- complete_theta(theta)
  
  if (is.null(input.u)) {
    check_distname(distname)
    check_theta(theta = theta, distname = distname)
    tau <- theta2tau(theta = theta, distname = distname)
    q.U <- function(p) qU(p, beta = theta$beta, distname = distname)
    distname.family <- get_distname_family(distname)
    is.non.negative <- get_distname_family(distname)$scale && 
      !get_distname_family(distname)$location
    # support of the LambertW RV; is bounded for skewed Lambert W x F distributions
    rv.support <- get_support(tau, is.non.negative = is.non.negative)
  } else {
    q.U <- input.u
    if (is.null(tau)) {
      stop("You must provide a 'tau' argument if 'input.u' is not NULL.")
    }
  }
    
  type.tmp <- tau2type(tau)
  
  if (type.tmp == "s") {
    # For skewed Lambert W x F distribution the quantile function must be obtained
    # by matching the inverse.  This is slow, but in general no closed form is available
    # (at least not for location-scale family).
    .compute_quantile <- function(prob) {
      if (prob == 0) {
        QQ <- rv.support[1]
      } else if (prob == 1) {
        QQ <- rv.support[2]
      } else if (prob > 0 && prob < 1) {
        # define auxiliary objective function as distance to alpha-level
        # then minimize quantile so it matches this alpha-level
        aux.p <- function(y.a) {
          return(lp_norm(pLambertW(y.a, theta = theta, distname = distname) - prob))
        }
        # use default (-10, 10) as values for standard Gaussian
        S.10 <- c(-10, 10) * tau["sigma_x"] + tau["mu_x"]
        if (is.non.negative) {
          # use 10 as maximum (given standardized exp has pexp(10, 1))
          S.10 <- c(0, 10) * tau["sigma_x"] + tau["mu_x"]
        }
        
        intv <- c(max(S.10[1], rv.support[1]), min(S.10[2], rv.support[2]))
        fit <- suppressWarnings(optimize(aux.p, interval = intv))
        QQ <- fit$min
      }  # end of 'if (prob>0 && prob<1)'
      names(QQ) <- NULL
      return(QQ)
    }  # end of 'aux' function
  } else if (type.tmp %in% c("hh", "h")) {
    # For a heavy-tailed Lambert W x  F distribution the quantile function can 
    # be obtained analytically.
    .compute_quantile <- function(prob) {
      if (prob == 0) {
        QQ <- rv.support[1]
      } else if (prob == 1) {
        QQ <- rv.support[2]
      } else if (prob > 0 && prob < 1) {
        u.alpha <- q.U(prob)
        if (type.tmp == "hh") {
          QQ.u <- G_2delta_2alpha(u.alpha, delta = theta$delta, alpha = theta$alpha)
        } else if (type.tmp == "h") {
          QQ.u <- G_delta_alpha(u.alpha, delta = theta$delta, alpha = theta$alpha)
        }
        QQ <- QQ.u * tau["sigma_x"] + tau["mu_x"]
      #} 
      #else if (prob > 0.5 && prob < 1) {
      #  u.alpha <- q.U(1 - prob)
      #  if (type.tmp == "hh") {
      #    QQ.u <- G_2delta_2alpha(u.alpha, delta = theta$delta, 
      #                            alpha = theta$alpha)
      #  } else if (type.tmp == "h") {
      #    QQ.u <- G_delta_alpha(u.alpha, delta = theta$delta, 
      #                          alpha = theta$alpha)
      #  }
      #  QQ <- QQ.u * tau["sigma_x"] + tau["mu_x"]
      #  QQ <- -QQ
      } else {
        stop("Probability is not correct:", prob, ".")
      }
      names(QQ) <- NULL
      return(QQ)
    }
  }
  # apply aux function to all p values
  return(sapply(p, .compute_quantile))
}