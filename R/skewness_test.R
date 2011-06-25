skewness_test <-
function(obj, method = "Wald"){

if (class(obj) != "LambertW_fit") obj = MLE_LambertW(obj, type="hh", distname="normal")

hessian = obj$hessian
theta.hat = obj$params.hat
KK = length(theta.hat)
var.theta.hat = -solve(hessian)
VV = var.theta.hat*length(obj$data)
DNAME = c(deparse(substitute(obj$data)))

if (method == "Wald"){
	RR = matrix(0, ncol=KK, nrow=1)
	RR[1,names(theta.hat) == c("delta_l", "delta_r")] = c(1,-1)
	WW =  ((RR %*% VV %*% t(RR)))^(-1) * t(theta.hat)%*% theta.hat
        method = "Wald test for symmetry (H_0: delta_l - delta_r = 0)"
	pval = 1-pchisq(WW,1)
	statistic = c(W = WW)
}

if (method == "ratio"){
        method = "Likelihood ratio test"
	LL_2h = obj$loglik.opt
	fit_1h = MLE_LambertW(obj$data, type="h", distname = obj$distname)
	LL_1h = fit_1h$loglik.opt

	lambda = 2*(LL_2h - LL_1h)
	pval = 1 - pchisq(lambda, 1)
	statistic = c(D = lambda)
}

RVAL <- list(statistic = statistic, p.value = pval, method = method, data.name = DNAME)
class(RVAL) <- "htest"
return(RVAL)
}
