IGMM.default <-
function (y, skewness_x = 0, type="s", kurtosis_x = 3, tau.0 = c(median(y), sd(y), (skewness(y) - 
    skewness_x)/6, delta_Taylor(y)), robust = FALSE, tol = .Machine$double.eps^0.25, location_family = TRUE)
{
     #if (is.null(tau.0)) tau.0 = c(median(y), sd(y), (skewness(y)-skewness_x)/6, max(0, (kurtosis(y)-3)/100)))

out = list() # this list will be returned
out$tol = tol
out$data = y
out$n = length(y)
out$type = type

theta = NULL # save theta (alpha, beta, gamma, delta) in a list format

if (!location_family) tau.0[1] = 0 # in case it is a scale Lambert W RV only (not centered)

# for skewed version
if (type =="s") {
    tau.0 = tau.0[-4]
    TAU = rbind(0, tau.0)
    colnames(TAU) = c("mu_x", "sigma_x","gamma")
    kk = 1
    iter = 0
    while (vec.norm(TAU[kk+1, ] - TAU[kk, ], p = 2) > tol && kk < 100) {

	mu.hat = TAU[kk+1,1]
	sigma.hat = TAU[kk+1,2]
	zz = (y - mu.hat)/sigma.hat

	DEL = gamma_GMM(zz, gamma.0 = TAU[kk+1,3], skewness_x = skewness_x, robust, tol=tol)
  gamma.hat = DEL$gamma
	
	uu = W_gamma(zz, gamma.hat)
    	xx = uu*sigma.hat + mu.hat
    	TAU = rbind(TAU, c(mean(xx), sqrt(var(xx)),gamma.hat))
        if (!location_family) TAU[nrow(TAU), 1] = 0 # for example for exponential input
	iter = iter + DEL$iterations
	kk = kk+1
    }

theta$gamma = gamma_GMM((y - TAU[nrow(TAU),1])/TAU[nrow(TAU),2], gamma.0 = TAU[kk+1,3], skewness_x = skewness_x, robust, tol=tol)$gamma

TAU = rbind(TAU, c(TAU[nrow(TAU),1], TAU[nrow(TAU),2], theta$gamma))
se = c(1, sqrt(1/2), 0.4)/sqrt(length(y))
#theta$gamma = TAU[nrow(TAU),3]
}

# for heavy-tail versions
if (type == "h"){
    tau.0 = tau.0[-3]
    TAU = rbind(0, tau.0)
    colnames(TAU) = c("mu_x", "sigma_x","delta")
    kk = 1
    iter = 0
    while (vec.norm(TAU[kk+1, ] - TAU[kk, ], p = 2) > tol && kk < 100) {

	mu.hat = TAU[kk+1,1]
	sigma.hat = TAU[kk+1,2]
	zz = (y - mu.hat)/sigma.hat

	DEL = delta_GMM(zz, delta.0 = TAU[kk+1,3], kurtosis_x = kurtosis_x, tol=tol)
        delta.hat = DEL$delta
	
	uu = W_delta(zz, delta.hat)
    	xx = uu*sigma.hat + mu.hat
    	TAU = rbind(TAU, c(mean(xx), sqrt(var(xx)),delta.hat))
        if (!location_family) TAU[nrow(TAU), 1] = 0 # for example for exponential input
	iter = iter + DEL$iterations
	kk = kk+1
    }
se = c(1, sqrt(1/2), 1)/sqrt(length(y))
#theta$delta = TAU[nrow(TAU),3]
theta$delta = delta_GMM((y - TAU[nrow(TAU),1])/TAU[nrow(TAU),2],delta.0 = TAU[nrow(TAU),3], kurtosis_x = kurtosis_x, tol=tol)$delta
TAU = rbind(TAU, c(TAU[nrow(TAU),1], TAU[nrow(TAU),2], theta$delta))
}

if (type == "hh"){
  print("Not implemented yet.")
}

    TAU = TAU[-1,]
    rownames(TAU) = c("Start",paste("Iteration", 2:nrow(TAU)-1))

    out$tau.0 = tau.0
    out$tau = TAU[nrow(TAU), ]
    out$TAU = TAU
    out$sub_iterations = kk
    out$iterations = iter
    out$theta = theta
    out$hessian = diag(-1/se^2)
    out$call = match.call()
    if (type == "s") out$distname = paste("Any distribution with theoretical skewness =", skewness_x, ".") 
    if (type == "h") out$distname = paste("Any distribution with theoretical kurtosis =", kurtosis_x, ".") 
    out$skewness_x = skewness_x
    out$kurtosis_x = kurtosis_x
    out$message = paste("Conversion reached after",kk,"steps.")
    out$method = c("IGMM")
    class(out) = "LambertW_fit"
    out
}

