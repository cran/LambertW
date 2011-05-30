loglik_penalty <-
function(theta, y = NULL, type = "h", distname = "normal"){
  yy = y
  theta = complete_theta(theta)
  if (any(type == c("h","hh"))) {
   tau = beta2tau(theta$beta, delta = theta$delta, distname = distname)
  }
  if (type == "s") {
    tau = beta2tau(theta$beta, gamma = theta$gamma, distname = distname)
  }
  
  zz = (yy - tau[1])/tau[2]
  
  if (type == "h") {
    if (length(theta$delta) > 1) stop("'delta' must have length 1. For asymmetric model use 'type = 'hh' '.")
    if (theta$delta == 0) return(0)
    uu = W_delta((yy - tau[1])/tau[2], delta = theta$delta)
    penalty = sum(log(uu/zz) - log(1+theta$delta * uu^2))
    #if (log == FALSE) penalty = exp(penalty)
    #penalty = uu / (zz *( 1 + theta$delta * uu^2) )
  }
  if (type == "hh") {
    if (all(theta$delta == 0)) return(0)
    uu = W_2delta((yy - tau[1])/tau[2], delta = theta$delta)
    penalty = sum(log(uu/zz)) - sum(log(1+theta$delta[1] * uu[uu < 0]^2)) - sum(log(1+theta$delta[2] * uu[uu > 0]^2))
    #if (log == FALSE) penalty = exp(penalty)
    #penalty = uu / (zz *( 1 + theta$delta * uu^2) )
  }
  if (type == "s"){
    if (theta$gamma == 0) return(0)
    if (any(distname == c("exp", "chisq", "gamma", "F"))) {
      penalty = sum(log(d1W(theta$gamma * zz)))
    }
    else penalty = NA
    #if (log == FALSE) penalty = exp(penalty)
  }
  return(penalty)
  }

