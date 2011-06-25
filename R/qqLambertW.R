qqLambertW <-
function(y, beta = c(0,1), gamma = 0, delta = 0, alpha = 1, distname = c("normal"), plot.it = TRUE, theta = NULL,...){
  if (!is.null(theta)){
    theta = complete_theta(theta)
    
    alpha = theta$alpha
    beta = theta$beta
    gamma = theta$gamma
    delta = theta$delta
  }
  check_theta(alpha = alpha, beta = beta, gamma = gamma, delta = delta, distname=distname)
  
  xlab = "Theoretical Quantiles"
  ylab = "Sample Quantiles"
  
  #tau = beta2tau(beta, distname=distname, gamma = gamma, delta = delta, alpha = alpha)
  
  main = paste("Lambert W x", distname,"QQ plot")
  
  y <- y[!is.na(y)]
  nn = length(y)
  
  if (nn == 0) stop("y is empty")
  
  p.n=ppoints(nn)
  x=qLambertW(p.n, beta=beta, alpha = alpha, gamma = gamma, delta = delta, distname = distname)
  sorted.x = sort(x)
  sorted.y = sort(y)
  if (plot.it) {
  	plot(sorted.x, sorted.y, main = main, xlab = xlab, ylab = ylab, ...)
     	abline(0,1)
  }
      invisible(list(x = sorted.x, y = sorted.y))
}
