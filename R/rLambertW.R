rLambertW <-
function(n = 1000, beta = c(0,1), gamma = 0, delta = 0, alpha = 1, distname = "normal", return.input = FALSE, input.U = NULL){

check_LambertW_parameters(alpha = alpha, gamma = gamma, delta=delta, beta=beta, distname=distname)

theta = beta2theta(beta, distname = distname, gamma = gamma, delta = delta)
if (is.null(input.U)) uu  = rU(n = n, beta = beta, distname=distname)
else uu = input.U

xx = uu * theta[2] + theta[1]
zz = uu
if (any(delta != 0)){
	if (length(delta) == 1) delta = c(delta, delta)
	zz[uu<0] = uu[uu<0] * exp(1/2*delta[1] * (uu[uu<0]^2)^alpha)
	zz[uu>=0] = uu[uu>=0] * exp(1/2*delta[2] * (uu[uu>=0]^2)^alpha)
}
if (gamma != 0){
zz = uu * exp(gamma * uu)
}
yy = zz * theta[2] + theta[1]

if (return.input) yy = list(x = xx, y = yy)
return(yy)
}

