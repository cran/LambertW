MLE_LambertW_new <-
function(y = NULL, distname = "normal", type = "s", 
                                theta.0 = list(), fixed_theta = list(alpha = 1), 
                                hessian = TRUE, estimate_only = FALSE, 
                                restricted = TRUE, optimization = "nlm") {
  yy = y
  if (length(theta.0) == 0) {
    theta.0 = starting_theta(yy, type = type, distname = distname)
  }
  if (any(type == c("h", "hh"))) {
    theta.0$delta[theta.0$delta <= 0] = 1e-05
  }
  if (type == "s") {
    if (is.na(theta.0$gamma)) {
      theta.0$gamma = 0
    }
    if (any(type == c("s")) & 
        all(distname != c("normal", "t", "cauchy", "unif"))) {
          if (theta.0$gamma <= 0) {
            theta.0$gamma = 0.00001
          }
    }
  }
  if (estimate_only) {
    hessian = FALSE
  }
  if (restricted) {
    theta.0 = restrict_theta(theta.0, distname = distname, type = type, inverse = TRUE)
  }
  out = list()
  out$data = yy
  out$theta.0 = theta.0
  out$params.0 = theta2params(theta.0)
  
  ## auxiliary function that computes likelihood
  aux_loglik = function(params = c(0, 1, 0.1), yy = NULL, data = NULL, 
                        distname = "normal", type = "h", restricted = TRUE) {
      theta = params2theta(params, type = type, distname = distname)
      if (!is.null(data)) {
        yy = data
      }
      if (restricted) {
        theta = restrict_theta(theta, distname = distname, type = type)
      }
      return(-loglik_LambertW(theta = theta, y = yy, type = type, 
          distname = distname)$loglik_LambertW)
  }
  if (optimization == "nlm") {
    fit = suppressWarnings(nlm(f = aux_loglik, p = out$params.0, 
                               yy = yy, type = type, distname = distname, 
                               restricted = restricted))
    out$params.hat = fit$estimate
    out$loglik.opt = -fit$minimum
    out$message = fit$message
  }
  if (optimization == "Rsolnp") {
    fit = suppressWarnings(solnp(theta2params(theta.0), fun = aux_loglik, 
                                 data = yy, type = type, distname = distname, 
                                 restricted = restricted, 
                                 control = list(trace = 0)))
    out$params.hat = fit$pars
    out$loglik.opt = -fit$values[length(fit$values)]
    out$message = fit$elapsed
  }
  out$theta = params2theta(out$params.hat, distname = distname, type = type)
  if (restricted) {
    out$theta = restrict_theta(out$theta, distname = distname, type = type)
  }
  names(out$theta) = names(theta.0)
  out$tau = beta2tau(out$theta$beta, gamma = out$theta$gamma, 
                     delta = out$theta$delta, alpha = 1)
  out$params.hat = theta2params(out$theta)

  if (estimate_only == TRUE) {
    return(out$params.hat)
  }
  if (any(type == c("h", "hh"))) {
    out$tau = beta2tau(beta = out$theta$beta, delta = out$theta$delta, 
                       gamma = 0, distname = distname)
  }
  if (type == "s") {
    out$tau = beta2tau(beta = out$theta$beta, delta = 0, 
                       gamma = 0, distname = distname)
  }
  HH = NULL
  if (hessian) {
    HH = -numericNHessian(aux_loglik, theta2params(out$theta), 
                          eps = 1e-06, yy = yy, type = type, distname = distname)
  }
  out$hessian = HH
  out$fisher = solve(-HH)
  out$loglik = aux_loglik
  out$call = match.call()
  out$distname = distname
  out$type = type
  out$method = "MLE"
  class(out) = "LambertW_fit"
  return(out)
}
