delta_GMM <-
function (z, kurtosis_x = 3,delta.0 = max(0, (kurtosis(z) - kurtosis_x)/100), tol=.Machine$double.eps^0.5){
 
obj.f = function(delta){
	u.g = W_delta(z, delta = delta)
        k4 = kurtosis(u.g)
	(k4 - kurtosis_x)^2
}

lb = 0
ub = 20
fit = nlminb(delta.0, obj.f, lower = lb, upper = ub, control=list(abs.tol=tol) )
delta.hat = fit$par
names(delta.hat) = NULL
out = NULL
out$delta = delta.hat
out$iterations = fit$iterations
return(out)
}

