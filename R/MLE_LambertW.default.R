MLE_LambertW.default <-
function (y, distname = c("normal"), type = "s", parameters.0 = list(), 
    fixed_parameters = list(alpha = 1), hessian = TRUE, estimate_only = FALSE) 
{
if (length(parameters.0) == 0) parameters.0 = starting_parameters(y, distname=distname, type=type, fixed_parameters = fixed_parameters)

if (estimate_only) hessian = FALSE
    param.0 = NULL
    for (ii in 1:length(parameters.0)) {
        param.0 = c(param.0, parameters.0[[ii]])
    }
    LambertW_logLH = function(param) {
        if (is.na(sum(param))) return(-10^6)
        beta = param[1:length(parameters.0$beta)]
        if (length(beta) == 2 && beta[2] < 0) beta[2] = 1
        if (any(type == c("h", "hh"))) {
            delta = param[-c(1:length(parameters.0$beta))]
            gamma = 0
        }
        if (type == "s") {
            gamma = param[-c(1:length(parameters.0$beta))]
            delta = 0
        }
        if (!is.null(fixed_parameters$alpha)) alpha = fixed_parameters$alpha
        else alpha = param["alpha"]

        g.log = log(dLambertW(y, beta = beta, gamma = gamma, delta = delta, alpha = alpha, distname = distname))
        L = sum(g.log)
        if (is.na(L)) L = -10^6
        if (L == Inf) L = -10^6
        L
    }
    LambertW_logLH(param.0)
    min.LambertW_logLH = function(param) {
        -LambertW_logLH(param)
    }
	
    lb = c(-Inf, rep(0, length(parameters.0$beta)-1))

    if (type == "s") lb = c(lb -Inf)
    if (type == "h") lb = c(lb, 0)
    if (type == "hh") lb = c(lb, 0, 0)
    if (is.null(fixed_parameters$alpha)) lb = c(lb, 0)

    location_family = FALSE
    if (any(distname == c("normal", "t", "cauchy", "laplace"))) location_family = TRUE
    if (!location_family) lb = lb[-1]

    fit = nlminb(param.0, min.LambertW_logLH, lower = lb)

    if (hessian) fit$hessian = numericNHessian(LambertW_logLH, t0 = fit$par)

    param.hat = fit$par
    names(param.hat) = names(param.0)

    if (estimate_only) return(param.hat)

    beta.hat = param.hat[1:length(parameters.0$beta)]
    if (type == "hh") delta.hat = param.hat[c("delta_l", "delta_r")]
    if (type == "h")  delta.hat = param.hat["delta"]
    if (type == "s")  gamma.hat = param.hat["gamma"]
    if (is.null(fixed_parameters$alpha)) alpha.hat = param.hat["alpha"]
    parameters.hat = NULL
    if (is.null(fixed_parameters$alpha)) parameters.hat$alpha = alpha.hat
    parameters.hat$beta = beta.hat
    if (any(type == c("h", "hh"))) parameters.hat$delta = delta.hat
    if (type == "s") parameters.hat$gamma = gamma.hat
    if (type == "s") theta.hat = beta2theta(beta.hat, gamma = gamma.hat, delta = 0, distname = distname)
    if (any(type == c("h", "hh"))) theta.hat = beta2theta(beta.hat, gamma = 0, delta = delta.hat, distname = distname)

    if (is.null(fixed_parameters$alpha)) theta.hat["alpha"] = parameters.hat$alpha

    #LW_output = create_LambertW_output(distname = distname, parameters = parameters.hat)

    est = NULL
    est$data = y
    est$loglik = LambertW_logLH
    est$loglik.opt = LambertW_logLH(param.hat)
    est$param.0 = param.0
    est$param.hat = param.hat
    est$parameters.0 = parameters.0
    est$parameters = parameters.hat
    est$beta = beta.hat
    est$theta = theta.hat
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

