loglik_LambertW <-
function(theta, y = NULL, distname = "normal", 
                                type = "h", return.neg.value = FALSE) {
  yy = y
  theta = complete_theta(theta)
  if (any(type == c("h", "hh"))) {
    tau = beta2tau(theta$beta, delta = theta$delta, distname = distname)
  }
  if (type == "s") { 
    tau = beta2tau(theta$beta, gamma = theta$gamma, distname = distname)
  }
  zz = (yy - tau[1])/tau[2]
  if (any(type == c("h", "hh"))) {
    uu = W_2delta(zz, delta = theta$delta)
  }
  if (type == "s") {
    uu = W_gamma(zz, gamma = theta$gamma)
  }
  xx = uu * tau[2] + tau[1]
  out = list()
  if (any(distname == c("t", "normal", "cauchy", "unif")) &
      type == "s" &
      theta$gamma != 0) {
        out$loglik_input = NA
        out$loglik_penalty = NA
        out$loglik = sum(log(dLambertW(yy, distname = distname, theta = theta)))
  } else {
    out$loglik_input = loglik_input(beta = theta$beta, x = xx, distname = distname)
    out$loglik_penalty = loglik_penalty(theta = theta, y = y, distname = distname, type = type)
    out$loglik_LambertW = out$loglik_input + out$loglik_penalty
  }
  if (return.neg.value) {
    return(-out$loglik_LambertW)
  } else {
    return(out)
  }
}
