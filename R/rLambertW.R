rLambertW <-
function(n = 1000, beta = c(0,1), gamma = 0, delta = 0, alpha = 1, distname = "normal", return.input = FALSE, input.U = NULL, theta = NULL){
  if (!is.null(theta)){
    theta = complete_theta(theta)
    
    alpha = theta$alpha
    beta = theta$beta
    gamma = theta$gamma
    delta = theta$delta
  }

  check_theta(alpha = alpha, gamma = gamma, delta=delta, beta=beta, distname=distname)
  
  tau = beta2tau(beta, distname = distname, gamma = gamma, delta = delta)
  if (is.null(input.U)) uu  = rU(n = n, beta = beta, distname=distname)
  else uu = input.U
  
  xx = uu * tau[2] + tau[1]
  zz = uu
  if (any(delta != 0)){
  	if (length(delta) == 1) delta = c(delta, delta)
  	zz[uu<0] = uu[uu<0] * exp(1/2*delta[1] * (uu[uu<0]^2)^alpha)
  	zz[uu>=0] = uu[uu>=0] * exp(1/2*delta[2] * (uu[uu>=0]^2)^alpha)
  }
  if (gamma != 0){
  zz = uu * exp(gamma * uu)
  }
  yy = zz * tau[2] + tau[1]
  names(yy) = NULL
  if (return.input) return( list(x = xx, y = yy) )
  else return(yy)
}

