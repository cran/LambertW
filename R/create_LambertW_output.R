create_LambertW_output <-
function(input = NULL, parameters = NULL, distname = input$distname){

if (is.null(input) & is.null(distname)) stop("The 'input' is missing. \n  Please generate an object of class 'LambertW_input' first and then pass it on as 'create_LambertW_output(input = my_LambertW_input, parameters = ...).")

if (is.null(parameters)) warning("You have not specified the parameters of the Lambert W RV transformation. \n  By default your LambertW output will be identical to the LambertW input.")

parameters = complete_LambertW_parameters(parameters, input = input)

alpha = parameters$alpha
beta = parameters$beta
gamma = parameters$gamma
delta = parameters$delta

if (is.null(distname) && !is.null(input)) distname = input$distname

if (!is.null(input)){
distname = input$distname
check_LambertW_parameters(alpha = alpha, beta = beta, gamma = gamma, delta = delta, distname = distname)
}

if (is.null(input)) {
input = create_LambertW_input(distname = distname, beta = beta)
}

theta = input$theta

if (gamma != 0) type = "s"
else if (any(delta != 0)){
	if (length(delta) == 1) type ="h"
	if (length(delta) == 2) type = "hh"
}
else type = ""

rY = function(parameters = obj$parameters) {
parameters = complete_LambertW_parameters(parameters, input = input)
aux = function(n){
rLambertW(beta = parameters$beta, delta = parameters$delta, gamma = parameters$gamma, alpha=parameters$alpha, input.U = input$rU(n), distname = input$distname)
}
return(aux)
}
#rY(parameters)(n=10)

#rY = function(n = NULL, parameters = obj$parameters) {
#rLambertW(beta = parameters$beta, delta = parameters$delta, gamma = parameters$gamma, alpha=parameters$alpha, input.U = input$rU(n), distname = input$distname)
#}


dY = function(parameters = obj$parameters){
parameters = complete_LambertW_parameters(parameters, input = input)
aux = function(y){
dLambertW(y, beta=parameters$beta, gamma = parameters$gamma, delta = parameters$delta, alpha=parameters$alpha, input.U = input$dU, distname = input$distname)
}
return(aux)
}
#plot(dY(parameters), -1,12)

#dY = function(y, parameters = obj$parameters){
#dLambertW(y, beta=parameters$beta, gamma = parameters$gamma, delta = parameters$delta, alpha=parameters$alpha, input.U = input$dU, distname = input$distname)
#}


pY = function(parameters = obj$parameters){
parameters = complete_LambertW_parameters(parameters, input = input)
aux = function(y){
pLambertW(y, beta=parameters$beta, gamma = parameters$gamma, delta = parameters$delta, alpha=parameters$alpha, input.U = input$pU, distname = input$distname)
}
return(aux)
}
#plot(pY(parameters), -1,12)

qY = function(parameters = obj$parameters){
parameters = complete_LambertW_parameters(parameters, input = input)
aux = function(p){
qLambertW(p, beta=parameters$beta, gamma = parameters$gamma, delta = parameters$delta, alpha=parameters$alpha, input.U = input$qU, distname = input$distname)
}
return(aux)
}


obj = NULL
obj$call = match.call()
obj$type = type
obj$dY = dY
obj$pY = pY
obj$rY = rY
obj$qY = qY
obj$beta = input$beta
obj$parameters = parameters
obj$theta = theta
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

