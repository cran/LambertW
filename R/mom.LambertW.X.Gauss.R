`mom.LambertW.X.Gauss` <-
function(theta){
names(theta)=NULL
delta=theta[1]
mu_x=theta[2]
sigma_x=theta[3]
######## moments of lambertW - Gaussian
mom.z=mom.LambertW.U.Gauss(delta)

mu_y=mu_x+sigma_x*mom.z$mu_z
sigma2_y=sigma_x^2*exp(delta^2)*((4*delta^2+1)*exp(delta^2)-delta^2)

list(mu_y=mu_y, sigma_y=sqrt(sigma2_y), skew=mom.z$skew, kurt=mom.z$kurt)
}

