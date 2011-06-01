beta2tau <-
function(beta, distname=c("normal"), gamma = 0, delta = 0, alpha = 1){
  tau = NULL
  
  check_theta(beta = beta, distname=distname)
  
  if (distname == "cauchy"){
     tau = beta
  }
  
  if (distname == "chisq"){
     tau[1] = 0
     tau[2] = 1
  }
  
  if (distname == "exp"){
     tau[1] = 0
     tau[2] = 1/beta
  }
  
  if (distname == "F"){
     tau[1] = 0
     tau[2] = (2*beta[2]^2*(beta[1]+beta[2] -2))/(beta[1]*(beta[2]-2)^2(beta[2]-4))
  }
  
  if (distname == "gamma"){
     if (length(beta) == 2) beta[3] = 1/beta[2]
     tau[1] = 0
     tau[2] = sqrt(beta[1]*beta[2]^2)
     }
  
  if (distname == "laplace"){
     tau[1] = beta[1]
     tau[2] = 2*beta[2]^2
  }
  
  if (distname == "normal") {
     tau[1:2] = beta
  }
  
  if (distname == "t") {
     nu = beta[3]
  	if (nu <=2) {
  		print(paste("A t-distribution with df =", nu ,"does not have finite variance. df has been set to 2.01"))
  		nu = 2.01
  		}
     ss = beta[2]
     fac = sqrt(nu/(nu - 2))
     sigma_x = ss*fac
     tau[1] = beta[1]
     tau[2] = sigma_x
  }
  
  
  if (distname == "unif") {
     tau[1] = 1/2*(beta[2] + beta[1])
     tau[2] = sqrt(1/12*(beta[2] - beta[1])^2)
  }
  
  if (length(delta) == 1) names(delta) = "delta"
  if (length(delta) == 2) names(delta) = c("delta_l", "delta_r")
  
  tau = c(tau, gamma, delta, alpha)
  
  names(tau)[1:3] = c("mu_x", "sigma_x","gamma")
  names(tau)[length(tau)] = "alpha"
  
  return(tau)
}

