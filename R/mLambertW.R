mLambertW <-
function(delta = 0, theta=c(delta,0,1), distname=c("normal")) {
if (length(delta) > 1) theta=delta; delta=theta[1]
if (distname=="normal") {
######## moments of lambertW - Gaussian
mom.LambertW.U.Gauss=function(delta){

m1=delta*exp((delta^2)/2)

m2=exp(2*delta^2)*(1 + 4*delta^2)
cm2=exp(delta^2)*(exp(delta^2)*(1 + 4*delta^2) - delta^2)

m3=exp((9/2)*delta^2)*(3*delta)*(9*delta^2+3)
cm3=m3-3*m1*m2+2*m1^3

m4=3*exp(8*delta^2)+96*delta^2*exp(8*delta^2)+256*delta^4*exp(8*delta^2)
cm4=m4-4*m1*m3+6*m1^2*m2^2-3*m1^4

skew=cm3/cm2^(3/2)
kurt=cm4/cm2^2
list(mu_z=m1, sigma2_z=cm2, skew=skew, kurt=kurt)
}

names(theta)=NULL
delta=theta[1]
mu_x=theta[2]
sigma_x=theta[3]
######## moments of lambertW - Gaussian
mom.z=mom.LambertW.U.Gauss(delta)

mu_y=mu_x+sigma_x*mom.z$mu_z
sigma2_y=sigma_x^2*exp(delta^2)*((4*delta^2+1)*exp(delta^2)-delta^2)

out = as.numeric(list(mu_y=mu_y, sigma_y=sqrt(sigma2_y), skew=mom.z$skew, kurt=mom.z$kurt-3))
names(out) = c("mu", "sigma", "gamma_1", "gamma_2")
return(out)
}
else print("Moments calculation for other than Normal distributions not supported yet!")
}

