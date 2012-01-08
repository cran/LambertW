create_LambertW_output <-
function(input = NULL, theta = NULL, distname = input$distname){
  
  if (is.null(input) & is.null(distname)) stop("The 'input' is missing. \n  Please generate an object of class 'LambertW_input' first and then pass it on as 'create_LambertW_output(input = my_LambertW_input, theta = ...).")
  
  if (is.null(theta)) warning("You have not specified the theta of the Lambert W RV transformation. \n  By default your LambertW output will be identical to the LambertW input.")
  
  theta = complete_theta(theta, input = input)
  
  alpha = theta$alpha
  beta = theta$beta
  gamma = theta$gamma
  delta = theta$delta
  
  if (is.null(distname) && !is.null(input)) distname = input$distname
  
  if (!is.null(input)){
    distname = input$distname
    check_theta(alpha = alpha, beta = beta, gamma = gamma, delta = delta, distname = distname)
  }
  
  if (is.null(input)) {
    input = create_LambertW_input(distname = distname, beta = beta)
  }
  
  #theta = input$theta
  
  if (gamma != 0) type = "s"
  else if (any(delta != 0)){
  	if (length(delta) == 1) type ="h"
  	if (length(delta) == 2) type = "hh"
  }
  else type = ""
  
  rY = function(theta = obj$theta) {
    theta = complete_theta(theta, input = input)
    aux = function(n){
      rLambertW(beta = theta$beta, delta = theta$delta, gamma = theta$gamma, alpha=theta$alpha, input.U = input$rU(n), distname = input$distname)
    }
    return(aux)
  }
  #rY(theta)(n=10)
  
  dY = function(theta = obj$theta){
    theta = complete_theta(theta, input = input)
    aux = function(y){
      dLambertW(y, beta=theta$beta, gamma = theta$gamma, delta = theta$delta, alpha=theta$alpha, input.U = input$dU, distname = input$distname)
    }
    return(aux)
  }
  #plot(dY(theta), -1,12)
  
  pY = function(theta = obj$theta){
    theta = complete_theta(theta, input = input)
    aux = function(y){
      pLambertW(y, beta=theta$beta, gamma = theta$gamma, delta = theta$delta, alpha=theta$alpha, input.U = input$pU, distname = input$distname)
    }
    return(aux)
  }
  #plot(pY(theta), -1,12)
  
  qY = function(theta = obj$theta){
    theta = complete_theta(theta, input = input)
    aux = function(p){
      qLambertW(p, beta=theta$beta, gamma = theta$gamma, delta = theta$delta, alpha=theta$alpha, input.U = input$qU, distname = input$distname)
    }
    return(aux)
  }
  
  
  obj = list()
  obj$call = match.call()
  obj$type = type
  obj$dY = dY
  obj$pY = pY
  obj$rY = rY
  obj$qY = qY
  obj$beta = input$beta
  obj$theta = theta
  obj$tau = beta2tau(beta = input$beta, distname=input$distname, gamma = theta["gamma"], delta = theta["delta"], alpha = theta["alpha"])
  obj$input_distname = input$distname
  obj$distname = paste("Lambert W x", input$distname)
  
  whole_distname = paste(obj$distname,"(", round(obj$beta[1],3), sep="")
  if (length(obj$beta) > 1) {
    for (i in 2:length(obj$beta)){
      whole_distname = paste(whole_distname,round(obj$beta[i],3),sep= ",")
    }
  }
  obj$distname_with_beta = paste(whole_distname,")", sep="")
  
  class(obj) = "LambertW_output"
  invisible(obj)
}
