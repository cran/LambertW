loglik_penalty <-
function (theta, y = NULL, type = "h", distname = "normal") 
{
  yy = y
  theta = complete_theta(theta)
  if (any(type == c("h", "hh"))) {
    tau = beta2tau(theta$beta, delta = theta$delta, distname = distname)
  }
  if (type == "s") {
    tau = beta2tau(theta$beta, gamma = theta$gamma, distname = distname)
  }
  zz = (yy - tau[1])/tau[2]
  if (type == "h") {
    if (length(theta$delta) > 1) {
      stop("'delta' must have length 1. For asymmetric model use 'type = 'hh' '.")
    }
    if (theta$delta == 0) {
      return(0)
    }
    uu = W_delta((yy - tau[1])/tau[2], delta = theta$delta)
    #penalty = sum(log(uu/zz) - log(1 + theta$delta * uu^2))
    penalty = sum(-theta$delta/2 * uu^2 - log(1 + theta$delta * uu^2))
  }
  if (type == "hh") {
    if (all(theta$delta == 0)){
      return(0)
    }
    uu = W_2delta((yy - tau[1])/tau[2], delta = theta$delta)
    ind = uu < 0
    penalty = sum(-theta$delta[1]/2 * uu[ind]^2) + 
              sum(-theta$delta[2]/2 * uu[!ind]^2) - 
              sum(log(1 + theta$delta[1] * uu[ind]^2)) -
              sum(log(1 + theta$delta[2] * uu[!ind]^2))
  }
  if (type == "s") {
    if (theta$gamma == 0) {
      return(0)
    }
    if (any(distname == c("exp", "chisq", "gamma", "F"))) {
      penalty = sum(log(d1W(theta$gamma * zz)))
    } else {
      penalty = NA
    }
  }
  return(penalty)
}
