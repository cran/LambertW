`MLE_LambertW.default` <-
function(y, distname=c("normal"),
theta.0=IGMM(y)$theta) {
####################################### Likelihood function
LambertW_logLH=function(theta){
delta=theta[1]
mu_x=theta[2]
sigma_x=theta[3]
nu=NULL
if (distname=="t") nu=theta[4]
################## standardize
L=0
z=(y-mu_x)/sigma_x

lb=-1/exp(1)/max(z)
ub=-1/exp(1)/min(z)

if (delta<=lb[1]) {
L=-500000
#print("Delta must be greater than the lower bound")
}
if (delta>=ub[1]) {
L=-500000
#print("Delta must be smaller than the upper bound")
}

g.log=log(dLambertW(y, theta, distname="normal"))
if (distname=="t") g.log=log(dLambertW(y, theta, distname="t"))

L=sum(g.log)
if (is.na(L)) L=-200000
return(L)
}

if (distname=="t") {
x.hat=get.input(y, theta.0)$x
t.fit=suppressWarnings(fitdistr(x.hat, distname))
theta.0[4]=t.fit$est[3]
names(theta.0)=c("delta", "mu_x", "sigma_x", "nu")
}

z=(y-theta.0[2])/theta.0[3]
lb=c(-1/exp(1)/max(z),-Inf, 0, 2.1)+0.00001
ub=c(-1/exp(1)/min(z), Inf, Inf, 100)-0.0001

min.LambertW_logLH=function(theta) {
-LambertW_logLH(theta)
}

fit=nlminb(min.LambertW_logLH, start=theta.0, lower = lb, upper = ub)
theta.hat=fit$par

#H=numericNHessian(f=LambertW_logLH, t0=theta.hat)

names(theta.hat)=c("delta", "mu_x", "sigma_x")
if (distname=="t") names(theta.hat)=c("delta", "mu_x", "sigma_x", "nu")

est=NULL
est$data = y
est$logLH = LambertW_logLH
est$theta.0 = theta.0
est$theta = theta.hat
#est$hessian = H
est$call = match.call()
est$distname = distname
est$message = fit$message
est$method = c("MLE")

class(est) = "LWest"

est
}

