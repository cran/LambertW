MLE_LambertW.default <-
function(y, distname=c("normal"),
theta.0=IGMM(y)$theta, hessian=TRUE) {
####################################### Likelihood function
LambertW_logLH=function(theta){

g.log=log(dLambertW(y, theta, distname))
L=sum(g.log)
if (is.na(L)) L=-10^6

r.y = range(y)
s.Y = support(theta)

if (r.y[1] <= s.Y[1] | r.y[2] >= s.Y[2]) {
	overlap = min(abs(r.y-s.Y))
	L = -(overlap*10+1)*10^4
}
L
}

if (distname=="t") {
x.hat=get.input(y, theta.0)
t.fit=suppressWarnings(fitdistr(x.hat, distname))
theta.0[4]=t.fit$est[3]
names(theta.0)=c("delta", "mu_x", "sigma_x", "nu")
}
else names(theta.0)=c("delta", "mu_x", "sigma_x")

min.LambertW_logLH=function(theta) {
-LambertW_logLH(theta)
}

fit=suppressWarnings(nlm(min.LambertW_logLH, p=theta.0, hessian=hessian))
names(fit$estimate) = names(theta.0)
est=NULL
est$data = y
est$logLH = LambertW_logLH
est$theta.0 = theta.0
est$theta = fit$estimate
est$hessian = fit$hessian
est$call = match.call()
est$distname = distname
est$message = fit$message
est$method = c("MLE")
class(est) = "LWest"

return(est)
}
