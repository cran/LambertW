create_LambertW_input <-
function (distname = NULL, beta = NULL, beta2theta = NULL, dU = NULL, 
    pU = NULL, rU = NULL, qU = NULL) 
{
    if (distname == "cauchy") {
        dU = function(u) dcauchy(u)
        rU = function(n) rcauchy(n = n)
        pU = function(u) pcauchy(u)
	qU = function(p) qcauchy(p)
        beta2theta = function(beta) theta = c(beta[1], beta[2], 
            0, 0)
    }
    if (distname == "chisq") {
	ss = sqrt(2 * beta)
        dU = function(u) dchisq(u*ss, df = beta)*ss
        rU = function(n) rchisq(n, df = beta)/ss
        pU = function(u) pchisq(u*ss, df = beta)
	qU = function(p) qchisq(p, df = beta)/ss
        beta2theta = function(beta) theta = c(0, sqrt(2*beta), 0, 0)
    }
    if (distname == "exp") {
        dU = function(u) dexp(u)
        rU = function(n) rexp(n)
        pU = function(u) pexp(u)
	qU = function(p) qexp(p)
        beta2theta = function(beta) theta = c(0, sqrt(1/beta), 
            0, 0)
    }
    if (distname == "normal") {
        dU = function(u) dnorm(u)
        rU = function(n) rnorm(n)
        pU = function(u) pnorm(u)
	qU = function(p) qnorm(p)
        beta2theta = function(beta) theta = c(beta[1], beta[2], 
            0, 0)
    }
    if (distname == "t") {
        nu = beta[3]
        sd_of_t = sqrt(nu/(nu - 2))
        dU = function(u) dt(u/sd_of_t, df = nu)/sd_of_t
        rU = function(n) rt(n = n, df = nu)/sd_of_t
        pU = function(u) pt(u/sd_of_t, df = nu)
	qU = function(p) qt(p/sd_of_t, df = nu)/sd_of_t
        beta2theta = function(beta) theta = c(beta[1], beta[2] * 
            sqrt(beta[3]/(beta[3] - 2)), 0, 0)
    }
    if (distname == "unif") {
        dU = function(u) dunif(u, -sqrt(12)/2, sqrt(12)/2)
        rU = function(n) runif(n, -sqrt(12)/2, sqrt(12)/2)
        pU = function(u) punif(u, -sqrt(12)/2, sqrt(12)/2)
	qU = function(p) qunif(p, -sqrt(12)/2, sqrt(12)/2)
        beta2theta = function(beta) theta = c(1/2 * (beta[2] + 
            beta[1]), sqrt(1/12 * (beta[2] - beta[1])^2), 0, 
            0)
    }


    theta = NULL
    if (!is.null(beta)) 
        theta = beta2theta(beta)
    dX = function(x) dU((x - theta[1])/theta[2])/theta[2]
    pX = function(x) pU((x - theta[1])/theta[2])
    rX = function(n) rU(n) * theta[2] + theta[1]
    qX = function(p) qU(p) * theta[2] + theta[1]
    names(beta) = beta_names(distname)

    obj = NULL
    obj$dU = dU
    obj$pU = pU
    obj$rU = rU
    obj$dX = dX
    obj$pX = pX
    obj$rX = rX
    obj$distname = "user"
    obj$beta = beta
    if (!is.null(distname)) obj$distname = distname
whole_distname = paste(obj$distname,"(", round(obj$beta[1],3), sep="")
if (length(obj$beta) > 1) {
for (i in 2:length(obj$beta)){
whole_distname = paste(whole_distname,round(obj$beta[i],3),sep= ",")
}
}
obj$distname_with_beta = paste(whole_distname,")", sep="")
    obj$beta2theta = beta2theta
    obj$theta = theta
    class(obj) = "LambertW_input"
    invisible(obj)
}

