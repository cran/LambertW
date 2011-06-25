gamma_01 <-
function(gamma, mu_y=0, sigma_y=1){
  if (gamma == 0) {
  sigma2_x = 1 
  mu_x = 0
  }
  ### for white noise WN(0,1) given gamma
  if (gamma!=0) {
    sigma2_x=sigma_y^2/(exp(gamma^2)*((4*gamma^2+1)*exp(gamma^2)-gamma^2))
    mu_x=mu_y-gamma*sqrt(sigma2_x)*exp(0.5*gamma^2)
  }
  c(mu_x, sqrt(sigma2_x), gamma)
}
