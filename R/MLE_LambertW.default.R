MLE_LambertW.default <-
function (y, distname = c("normal"), type = "s", theta.0 = list(), 
    fixed_theta = list(alpha = 1), hessian = TRUE, estimate_only = FALSE) 
{
if (length(theta.0) == 0) theta.0 = starting_theta(y, distname=distname, type=type, fixed_theta = fixed_theta)

if (estimate_only) hessian = FALSE
    param.0 = NULL
    for (ii in 1:length(theta.0)) {
        param.0 = c(param.0, theta.0[[ii]])
    }
    LambertW_logLH = function(param) {
        if (is.na(sum(param))) return(-10^6)
        beta = param[1:length(theta.0$beta)]
        if (length(beta) == 2 && beta[2] < 0) beta[2] = 1
        if (any(type == c("h", "hh"))) {
            delta = param[-c(1:length(theta.0$beta))]
            gamma = 0
        }
        if (type == "s") {
            gamma = param[-c(1:length(theta.0$beta))]
            delta = 0
        }
        if (!is.null(fixed_theta$alpha)) {alpha = fixed_theta$alpha}
        else {alpha = param["alpha"]}

        g.log = log(dLambertW(y, beta = beta, gamma = gamma, delta = delta, alpha = alpha, distname = distname))
        L = sum(g.log)
        if (is.na(L)) L = -10^6
        if (L == Inf) L = -10^6
        L
    }

    min.LambertW_logLH = function(param) {
        -LambertW_logLH(param)
    }
	
    lb = bounds_theta(type=type, distname=distname, beta = theta.0$beta, fixed_theta = fixed_theta)$lowerbound
    ub = bounds_theta(type=type, distname=distname, beta = theta.0$beta, fixed_theta = fixed_theta)$upperbound	
    #print(lb)
    fit = nlminb(param.0, min.LambertW_logLH, lower = lb, upper=ub, control = list(trace = 0))
    #print(param.0)
    #fit = constrOptim(param.0, min.LambertW_logLH, grad = NULL, ui=diag(length(param.0)), ci=lb)
    #fit = nlminb(param.0, min.LambertW_logLH, lower = lb, upper=ub, control = list(trace = 1))

    if (hessian) fit$hessian = numericNHessian(LambertW_logLH, t0 = fit$par)

    params.hat = fit$par
    names(params.hat) = names(param.0)

    if (estimate_only) return(params.hat)

    beta.hat = params.hat[1:length(theta.0$beta)]
    if (type == "hh") delta.hat = params.hat[c("delta_l", "delta_r")]
    if (type == "h")  delta.hat = params.hat["delta"]
    if (type == "s")  gamma.hat = params.hat["gamma"]
    if (is.null(fixed_theta$alpha)) alpha.hat = params.hat["alpha"]
    parameters.hat = NULL
    if (is.null(fixed_theta$alpha)) parameters.hat$alpha = alpha.hat
    parameters.hat$beta = beta.hat

    if (type == "s") parameters.hat$gamma = gamma.hat
    if (any(type == c("h", "hh"))) parameters.hat$delta = delta.hat

    if (type == "s") tau.hat = beta2tau(beta.hat, gamma = gamma.hat, delta = 0, distname = distname)
    if (any(type == c("h", "hh"))) tau.hat = beta2tau(beta.hat, gamma = 0, delta = delta.hat, distname = distname)

    if (is.null(fixed_theta$alpha)) tau.hat["alpha"] = parameters.hat$alpha

    #LW_output = create_LambertW_output(distname = distname, parameters = parameters.hat)

    est = NULL
    est$data = y
    est$loglik = LambertW_logLH
    est$loglik.opt = LambertW_logLH(params.hat)
    est$param.0 = param.0
    est$params.hat = params.hat
    est$theta.0 = theta.0
    est$theta = parameters.hat
    est$beta = beta.hat
    est$tau = tau.hat
    est$type = type
    est$hessian = fit$hessian
    est$call = match.call()
    #est$LambertW_output = LW_output
    est$distname = distname
    est$message = fit$message
    est$method = c("MLE")
    class(est) = "LambertW_fit"
    return(est)
}

