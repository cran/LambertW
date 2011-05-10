delta_GMM <-
function (z, kurtosis_x = 3, delta.0 = delta_Taylor(z), tol=.Machine$double.eps^0.25){   
  obj.f = function(delta){
  	u.g = W_delta(z, delta = delta)
    k4 = kurtosis(u.g)
  	(k4 - kurtosis_x)^2
  }
  
  lb = 0
  ub = 100
  fit = nlminb(delta.0, obj.f, lower = lb, upper = ub, control=list(abs.tol=tol) )
  delta.hat = fit$par
  names(delta.hat) = NULL
  
  out = list()
  out$delta = delta.hat
  out$iterations = fit$iterations
  return(out)
}

