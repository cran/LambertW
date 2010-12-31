beta2theta <-
function(beta, distname=c("normal"), gamma = 0, delta = 0, alpha = 1){
theta_pre = NULL

check_LambertW_parameters(beta = beta, distname=distname)

if (distname == "cauchy"){
theta_pre = beta
}

if (distname == "chisq"){
theta_pre[1] = 0
theta_pre[2] = sqrt(2*beta)
}

if (distname == "exp"){
theta_pre[1] = 0
theta_pre[2] = sqrt(1/beta)
}

if (distname == "F"){
theta_pre[1] = 0
theta_pre[2] = (2*beta[2]^2*(beta[1]+beta[2] -2))/(beta[1]*(beta[2]-2)^2(beta[2]-4))
}

if (distname == "laplace"){
theta_pre[1] = beta[1]
theta_pre[2] = 2*beta[2]^2
}

if (distname == "normal") {
theta_pre[1:2] = beta
}

if (distname == "t") {
nu = beta[3]
	if (nu <=2) {
		print(paste("A t-distribution with df =", nu ,"does not have finite variance. df has been set to 2.01"))
		nu = 2.01
		}
ss = beta[2]
fac = sqrt(nu/(nu - 2))
sigma_x = ss*fac
theta_pre[1] = beta[1]
theta_pre[2] = sigma_x
}


if (distname == "unif") {
theta_pre[1] = 1/2*(beta[2] + beta[1])
theta_pre[2] = sqrt(1/12*(beta[2] - beta[1])^2)
}

if (length(delta) == 1) names(delta) = "delta"
if (length(delta) == 2) names(delta) = c("delta_l", "delta_r")

theta_pre = c(theta_pre, gamma, delta, alpha)

names(theta_pre)[1:3] = c("mu_x", "sigma_x","gamma")
names(theta_pre)[length(theta_pre)] = "alpha"

return(theta_pre)
}

