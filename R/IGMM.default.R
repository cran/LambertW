IGMM.default <-
function (y, skewness_x = 0, type="s", kurtosis_x = 3, theta.0 = c(median(y), sd(y), (skewness(y) - 
    skewness_x)/6, max(0, (kurtosis(y)-3)/100)), robust = FALSE, tol = .Machine$double.eps^0.5, location_family = TRUE)
{
     #if (is.null(theta.0)) theta.0 = c(median(y), sd(y), (skewness(y)-skewness_x)/6, max(0, (kurtosis(y)-3)/100)))

out = list() # this list will be returned
out$tol = tol
out$data = y
out$type = type
out$n = length(y)

parameters = NULL # save parameters (alpha, beta, gamma, delta) in a list format

if (!location_family) theta.0[1] = 0 # in case it is a scale Lambert W RV only (not centered)

# for skewed version
if (type =="s") {
    theta.0 = theta.0[-4]
    THETA = rbind(0, theta.0)
    colnames(THETA) = c("mu_x", "sigma_x","gamma")
    kk = 1
    iter = 0
    while (vec.norm(THETA[kk+1, ] - THETA[kk, ], p = 2) > tol && kk < 100) {

	mu.hat = THETA[kk+1,1]
	sigma.hat = THETA[kk+1,2]
	zz = (y - mu.hat)/sigma.hat

	DEL = gamma_GMM(zz, gamma.0 = THETA[kk+1,3], skewness_x = skewness_x, robust, tol=tol)
        gamma.hat = DEL$gamma
	
	uu = W_gamma(zz, gamma.hat)
    	xx = uu*sigma.hat + mu.hat
    	THETA = rbind(THETA, c(mean(xx), sqrt(var(xx)),gamma.hat))
        if (!location_family) THETA[nrow(THETA), 1] = 0 # for example for exponential input
	iter = iter + DEL$iterations
	kk = kk+1
    }
se = c(1, sqrt(1/2), 0.4)/sqrt(length(y))
parameters$gamma = THETA[nrow(THETA),3]
}

# for heavy-tail versions
if (type == "h"){
    theta.0 = theta.0[-3]
    THETA = rbind(0, theta.0)
    colnames(THETA) = c("mu_x", "sigma_x","delta")
    kk = 1
    iter = 0
    while (vec.norm(THETA[kk+1, ] - THETA[kk, ], p = 2) > tol && kk < 100) {

	mu.hat = THETA[kk+1,1]
	sigma.hat = THETA[kk+1,2]
	zz = (y - mu.hat)/sigma.hat

	DEL = delta_GMM(zz, delta.0 = THETA[kk+1,3], kurtosis_x = kurtosis_x, tol=tol)
        delta.hat = DEL$delta
	
	uu = W_delta(zz, delta.hat)
    	xx = uu*sigma.hat + mu.hat
    	THETA = rbind(THETA, c(mean(xx), sqrt(var(xx)),delta.hat))
        if (!location_family) THETA[nrow(THETA), 1] = 0 # for example for exponential input
	iter = iter + DEL$iterations
	kk = kk+1
    }
se = c(1, sqrt(1/2), 0.4)/sqrt(length(y))
parameters$delta = THETA[nrow(THETA),3]
}

if (type == "hh"){
print("Not implemented yet.")
}

    THETA = THETA[-1,]
    rownames(THETA) = c("Start",paste("Iteration", 2:nrow(THETA)-1))

    out$theta.0 = theta.0
    out$theta = THETA[nrow(THETA), ]
    out$THETA = THETA
    out$sub_iterations = kk-1
    out$iterations = iter
    out$parameters = parameters
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

