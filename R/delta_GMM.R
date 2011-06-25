delta_GMM <-
function (z, kurtosis_x = 3, skewness_x = 0, type ="h", delta.0 = delta_Taylor(z), tol=.Machine$double.eps^0.25, restricted = TRUE, bounds = c(-5,10)){   
  if (restricted == TRUE) bounds[1] = 0
  
  if (type == "h"){
    obj.f = function(delta){
      u.g = W_delta(z, delta = delta)
      k4 = kurtosis(u.g)
      #k4 = mean(u.g^4)
    	(k4 - kurtosis_x)^2
    }
    lb = bounds[1]
    ub = bounds[2]
    fit = nlminb(delta.0, obj.f, lower = lb, upper = ub, control=list(abs.tol=tol, trace = 0) )
  }
  if (type == "hh"){
    obj.f = function(delta){
      #print(paste("deltaGMM tries", delta))
      u.g = W_2delta(z, delta = delta)

      k4 = kurtosis(u.g)
      s3 = skewness(u.g)
    	(k4 - kurtosis_x)^2 + (s3 - skewness_x)^2
    }
    delta.0 = c(delta.0*1.2, delta.0*0.8)
    if (skewness(z) > 0) delta.0 = delta.0[2:1]
    
    lb = rep(bounds[1],2)
    ub = rep(bounds[2],2)
    fit = nlminb(delta.0, obj.f, lower = lb, upper = ub, control=list(abs.tol=tol, trace = 0) )
 
  }
  #fit = nlm(p = delta.0, f = obj.f, gradtol = tol)
  #fit$par = fit$est
  #if (restricted == TRUE) fit$par = exp(fit$par)
  delta.hat = fit$par
  names(delta.hat) = NULL
  
  out = list()
  out$delta = round(delta.hat, 6)
  out$iterations = fit$iterations
  return(out)
}
