delta.01 <-
function(delta, mu_y=0, sigma_y=1){
if (delta == 0) {
sigma2_x=1 
mu_x=0
}
### for white noise WN(0,1) given delta
if (delta!=0) {
sigma2_x=sigma_y^2/(exp(delta^2)*((4*delta^2+1)*exp(delta^2)-delta^2))
mu_x=mu_y-delta*sqrt(sigma2_x)*exp(0.5*delta^2)
}
theta = c(delta, mu_x, sqrt(sigma2_x))
theta
}

