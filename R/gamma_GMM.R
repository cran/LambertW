gamma_GMM <-
function (z, skewness_x = 0,gamma.0 = gamma_Taylor(z), robust = FALSE, tol=.Machine$double.eps^0.25, restricted = FALSE) {
    obj.f = function(gamma) {
        u.d = W_gamma(z, gamma)
        if (!robust) s3 = skewness(u.d)
        else s3 = mc(u.d)
        (s3 - skewness_x)^2
    }
    lb = -1/exp(1)/max(z) + 1e-05
    if (restricted == TRUE) lb = 0
    ub = -1/exp(1)/min(z) - 1e-05
    fit = nlminb(gamma.0, obj.f, lower=lb, upper=ub, control=list(abs.tol=tol))

    gamma.hat = fit$par
    
    out = list()
    out$gamma = gamma.hat
    out$iterations = fit$iterations
    return(out)
}
