`mom.LambertW.U.Gauss` <-
function(delta){
######## moments of lambertW - Gaussian
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

