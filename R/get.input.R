get.input <-
function(y, theta, return.u=FALSE) {
if (is.na(theta["alpha"])) {
	theta = c(theta, 1)
	names(theta)[length(theta)] = "alpha"
}
if (is.na(theta["gamma"])) {
	theta = c(theta[1:2], 0, theta[-c(1:2)])
	names(theta)[3] = "gamma"
}

if (is.na(theta["delta"]) && is.na(theta["delta_l"])) {
	theta = c(theta[1:3], 0, theta[-c(1:3)])
	names(theta)[4] = "delta"
}

cc=theta[1]
ss=theta[2]
gamma = theta["gamma"]
delta = theta[-c(1:3)][-length(theta[-c(1:3)])]
alpha = theta["alpha"]

zz=(y-cc)/ss
if (gamma != 0 & all(delta == 0)) uu = W_gamma(zz, gamma)
if (any(delta != 0) & gamma == 0) uu = W_2delta_alpha(zz, delta = delta, alpha=alpha)
if (all(delta == 0) && gamma == 0) uu = zz
xx=uu*ss+cc

if (return.u)  {
nu=theta[4]
if (!is.na(nu)) u=sqrt(nu/(nu-2))*uu
O=NULL
O$u=uu
O$x=xx
return(O)
}
else return(xx)
}

