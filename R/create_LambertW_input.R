create_LambertW_input <-
function (distname = NULL, beta = NULL, beta2tau = NULL, dU = NULL, pU = NULL, rU = NULL, qU = NULL){
    if (distname == "cauchy") {
        dU = function(u) dcauchy(u)
        rU = function(n) rcauchy(n = n)
        pU = function(u) pcauchy(u)
	      qU = function(p) qcauchy(p)
        beta2tau = function(beta) tau = c(beta[1], beta[2], 0, 0)
    }
    if (distname == "chisq") {
        dU = function(u) dchisq(u, df = beta)
        rU = function(n) rchisq(n, df = beta)
        pU = function(u) pchisq(u, df = beta)
	      qU = function(p) qchisq(p, df = beta) 
        beta2tau = function(beta) tau = c(0, 1, 0, 0)
    }
    if (distname == "exp") {
        dU = function(u) dexp(u)
        rU = function(n) rexp(n)
        pU = function(u) pexp(u)
	      qU = function(p) qexp(p)
        beta2tau = function(beta) tau = c(0, sqrt(1/beta), 0, 0)
    }
    if (distname == "normal") {
        dU = function(u) dnorm(u)
        rU = function(n) rnorm(n)
        pU = function(u) pnorm(u)
	      qU = function(p) qnorm(p)
        beta2tau = function(beta) tau = c(beta[1], beta[2], 0, 0)
    }
    if (distname == "t") {
        nu = beta[3]
        sd_of_t = sqrt(nu/(nu - 2))
        dU = function(u) dt(u/sd_of_t, df = nu)/sd_of_t
        rU = function(n) rt(n = n, df = nu)/sd_of_t
        pU = function(u) pt(u/sd_of_t, df = nu)
	      qU = function(p) qt(p/sd_of_t, df = nu)/sd_of_t
        beta2tau = function(beta) tau = c(beta[1], beta[2] * sqrt(beta[3]/(beta[3] - 2)), 0, 0)
    }
    if (distname == "unif") {
        dU = function(u) dunif(u, -sqrt(12)/2, sqrt(12)/2)
        rU = function(n) runif(n, -sqrt(12)/2, sqrt(12)/2)
        pU = function(u) punif(u, -sqrt(12)/2, sqrt(12)/2)
	      qU = function(p) qunif(p, -sqrt(12)/2, sqrt(12)/2)
        beta2tau = function(beta) tau = c(1/2 * (beta[2] + beta[1]), sqrt(1/12 * (beta[2] - beta[1])^2), 0, 
            0)
    }


    tau = NULL
    if (!is.null(beta)) tau = beta2tau(beta)
    #dX = function(x) dU((x - tau[1])/tau[2])/tau[2]
    #pX = function(x) pU((x - tau[1])/tau[2])
    #rX = function(n) rU(n) * tau[2] + tau[1]
    #qX = function(p) qU(p) * tau[2] + tau[1]
    names(beta) = beta_names(distname)

    obj = list()
    obj$beta = beta
    obj$distname = "user"
    if (!is.null(distname)) obj$distname = distname

    rX = function(beta = obj$beta, distname = obj$distname) {
      tau = beta2tau(beta)
      aux = function(n){
        rU(n) * tau[2] + tau[1]
      }
      return(aux)
    }
    pX =  function(beta = obj$beta, distname = obj$distname) {
      tau = beta2tau(beta)
      aux = function(x){
        pU((x - tau[1])/tau[2])
      }
      return(aux)
    }
    dX =  function(beta = obj$beta, distname = obj$distname) {
      tau = beta2tau(beta)
      aux = function(x){
        dU((x - tau[1])/tau[2])
      }
      return(aux)
    }
    qX = function(beta = obj$beta, distname = obj$distname) {
      tau = beta2tau(beta)
      aux = function(p){
        qU(p) * tau[2] + tau[1]
      }
      return(aux)
    }


    obj$dU = dU
    obj$pU = pU
    obj$rU = rU
    obj$dX = dX
    obj$pX = pX
    obj$rX = rX


    whole_distname = paste(obj$distname,"(", round(obj$beta[1],3), sep="")
    if (length(obj$beta) > 1) {
      for (i in 2:length(obj$beta)){
        whole_distname = paste(whole_distname,round(obj$beta[i],3),sep= ",")
      }
    }
    obj$distname_with_beta = paste(whole_distname,")", sep="")
    obj$beta2tau = beta2tau
    obj$tau = tau
    class(obj) = "LambertW_input"
    invisible(obj)
}

