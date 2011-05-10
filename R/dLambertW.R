dLambertW <-
function (y, beta = c(0,1), gamma = 0, delta = 0, alpha = 1, distname = c("normal"), input.U = NULL, theta = NULL) 
{
  if (!is.null(theta)){
    theta = complete_theta(theta)
    
    alpha = theta$alpha
    beta = theta$beta
    gamma = theta$gamma
    delta = theta$delta
  }
  tau = beta2tau(beta, distname=distname, gamma=gamma, delta=delta, alpha = alpha)
  mu_x = tau[1]
  sigma_x = tau[2]
  
  if (is.null(input.U)) f_U = function(u) dU(u, beta=beta, distname=distname)
  else f_U = input.U
  
  f_X = function(x) f_U((x - mu_x)/sigma_x)/sigma_x
  
  if (all(delta == 0) && gamma == 0) {
  g = f_X(y)
  names(g) = NULL
  g
  }
  else { # begin of else
  g = rep(NA, length(y))
  z = (y - mu_x)/sigma_x
  names(z) = NULL
  ## the heavy-tail version (if delta != 0)
  if (any(delta !=0)){
  	if (length(delta) == 1){
  		#u = W_delta(z, delta=delta, sign=TRUE)
  		u = W_delta_alpha(z, delta=delta, alpha = alpha)
  		#g = f_U(u) * d1W_delta(z, delta=delta) * sigma_x
  		#g = 1/sigma_x * sign(z) * f_U(sign(z) * u) * u / (z*(1 + delta*u^2)) #d1W_delta(z, delta=delta) * sigma_x
  		g = 1/sigma_x * f_U(u) * d1W_delta_alpha(z, delta=delta, alpha=alpha)
  	}
  	if (length(delta) == 2){
  		ind.pos = (z > 0)
  		if (length(alpha) == 1) alpha = c(alpha, alpha)
  		g[ind.pos] = dLambertW(y[ind.pos], beta=beta, gamma = 0, delta=delta[2], alpha = alpha[2], distname=distname)
  		g[!ind.pos] = dLambertW(y[!ind.pos], beta=beta, gamma = 0, delta=delta[1], alpha = alpha[1], distname=distname)	
  	}
  } # end of "if (any(delta !=0))"
  if (any(gamma != 0)){
  if (gamma < 0) {
          y = -y
          gamma = -gamma
          mu_x = -mu_x
      }
  	z = (y - mu_x)/sigma_x
  	names(z) = NULL
  	r_0 = W_gamma(z, gamma = gamma)
      	r_1 = W_gamma_1(z, gamma = gamma)
      	x_0 = r_0 * sigma_x + mu_x
      	x_1 = r_1 * sigma_x + mu_x
        g_0 = f_X(x_0) * d1W(gamma * z)
        g_1 = f_X(x_0) * d1W(gamma * z) - f_X(x_1) * d1W_1(gamma * z)
        g = g_0 * as.numeric(z >= 0) + g_1 * as.numeric(z < 0)
        g[is.na((g < -1))] = 0
  } # end of "if (any(gamma != 0))"
  
  } # end of else 
  names(g) = NULL
  g
}

