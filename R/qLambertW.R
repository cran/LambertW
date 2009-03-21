`qLambertW` <-
function(p, theta, distname="normal") {
delta=theta[1]
mu_x=theta[2]
sigma_x=theta[3]
nu=theta[4]

aux=function(y.a) {
(pLambertW(y.a, theta, distname)-p)^2
}
S=support(theta)
S=c(-10,10)*sigma_x+mu_x
fit=suppressWarnings(optimize(aux, interval=S))
fit$min
}

