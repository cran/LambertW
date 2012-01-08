pLambertW <-
function (q, beta = c(0,1), gamma = 0, delta = 0, alpha = 1, distname = c("normal"), input.U = NULL, theta = NULL) 
{
  if (!is.null(theta)){
    theta = complete_theta(theta)
    
    alpha = theta$alpha
    beta = theta$beta
    gamma = theta$gamma
    delta = theta$delta
  }  
  
  
  y = q
  tau = beta2tau(beta, distname=distname, gamma=gamma, delta=delta)
  mu_x = tau[1]
  sigma_x = tau[2]
  
  
  if (is.null(input.U)) F_U = function(u) pU(u, beta=beta, distname=distname)
  else F_U = input.U
  
  F_X = function(x) F_U((x - mu_x)/sigma_x)
  
  if (length(delta) == 1 && delta == 0 && gamma == 0) return(F_X(y))
  else { # begin of else
  G = rep(NA, length(y))
  z = (y - mu_x)/sigma_x
  names(z) = NULL
  ## the heavy-tail version (if delta != 0)
  if (any(delta !=0)){
  	if (length(delta) == 1){
  		u = W_delta_alpha(z, delta=delta, alpha = alpha)
  		G = F_U(u)
  	}
  	if (length(delta) == 2){
  		ind.pos = (z > 0)
  		if (length(alpha) == 1) alpha = c(alpha, alpha)
  		G[ind.pos] = pLambertW(y[ind.pos], beta=beta, gamma = 0, delta=delta[2], alpha = alpha[2], distname=distname)
  		G[!ind.pos] = pLambertW(y[!ind.pos], beta=beta, gamma = 0, delta=delta[1], alpha = alpha[1], distname=distname)	
  	}
  } # end of "if (any(delta !=0))"
  if (any(gamma != 0)){
  gamma_negative = FALSE
  	if (gamma < 0) {
          	y = -y
          	gamma = -gamma
          	mu_x = -mu_x
  		gamma_negative = TRUE
      	}
  	z = (y - mu_x)/sigma_x
  	names(z) = NULL
  	r_0 = W_gamma(z, gamma = gamma)
      	r_1 = W_gamma_1(z, gamma = gamma)
      	x_0 = r_0 * sigma_x + mu_x
      	x_1 = r_1 * sigma_x + mu_x
  
        G_0 = F_X(x_0)
        G_1 = F_X(x_0) - F_X(x_1)
        #G = G_0 * as.numeric(z >= 0) + G_1 * as.numeric(z < 0)
  	G[z>=0] = G_0[z>=0] 
  	G[z<0] = G_1[z<0]
        G[is.na((G < -1))] = 0
  	if (gamma_negative) G = 1 - G
  } # end of "if (any(gamma != 0))"
  
  } # end of else 
  names(G) = NULL
  return(G)
}
