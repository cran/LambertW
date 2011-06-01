create_LambertW_input <-
function (distname = NULL, beta = NULL, beta2tau = NULL, dU = NULL, pU = NULL, rU = NULL, qU = NULL){
    if (distname == "normal") {
        dU_temp = function(u) dnorm(u)
        rU_temp = function(n) rnorm(n)
        pU_temp = function(u) pnorm(u)
	      qU_temp = function(p) qnorm(p)
        beta2tau_temp = function(beta) c(beta[1], beta[2], 0, 0, 1)
    }
    else{
      dU_temp = function(u) dU(u, beta = beta, distname = distname)
      rU_temp = function(u) rU(u, beta = beta, distname = distname)
      pU_temp = function(u) pU(u, beta = beta, distname = distname)
      qU_temp = function(u) qU(u, beta = beta, distname = distname)
      beta2tau_temp = function(beta) beta2tau(beta = beta, distname = distname)
    }

    tau = NULL
    if (!is.null(beta)) tau = beta2tau_temp(beta)
    names(beta) = beta_names(distname)

    obj = list()
    obj$beta = beta
    obj$distname = "user"
    if (!is.null(distname)) obj$distname = distname

    rX = function(beta = obj$beta, distname = obj$distname) {
      tau = beta2tau(beta = beta, distname = distname)
      aux = function(n){
        rU(n, beta = beta, distname = distname) * tau[2] + tau[1]
      }
      return(aux)
    }
    pX =  function(beta = obj$beta, distname = obj$distname) {
      tau = beta2tau(beta = beta, distname = distname)
      aux = function(x){
        pU((x - tau[1])/tau[2], beta = beta, distname = distname)
      }
      return(aux)
    }
    dX =  function(beta = obj$beta, distname = obj$distname) {
      tau = beta2tau(beta = beta, distname = distname)
      aux = function(x){
        dU((x - tau[1])/tau[2], beta = beta, distname = distname)
      }
      return(aux)
    }
    qX = function(beta = obj$beta, distname = obj$distname) {
      tau = beta2tau(beta = beta, distname = distname)
      aux = function(p){
        qU(p, beta = beta, distname = distname) * tau[2] + tau[1]
      }
      return(aux)
    }


    obj$dU = dU_temp
    obj$pU = pU_temp
    obj$rU = rU_temp
    obj$qU = qU_temp
    obj$dX = dX
    obj$pX = pX
    obj$rX = rX
    obj$qX = qX

    whole_distname = paste(obj$distname,"(", round(obj$beta[1],3), sep="")
    if (length(obj$beta) > 1) {
      for (i in 2:length(obj$beta)){
        whole_distname = paste(whole_distname,round(obj$beta[i],3),sep= ",")
      }
    }
    obj$distname_with_beta = paste(whole_distname,")", sep="")
    obj$beta2tau = beta2tau_temp
    obj$tau = tau
    class(obj) = "LambertW_input"
    invisible(obj)
}

