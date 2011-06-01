theta2tau <-
function(theta = list(beta = c(0,1)), distname = "normal"){
  theta = complete_theta(theta)
  beta2tau(theta$beta, gamma = theta$gamma, delta = theta$delta, alpha = theta$alpha, distname = distname)
  }

