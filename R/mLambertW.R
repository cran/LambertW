mLambertW <-
function(beta = c(0, 1), distname = c("normal"), gamma = 0, delta = 0, alpha = 1, theta = NULL) 
{
if (distname != "normal") stop("Moments calculation for other than Normal distributions are not supported yet!")

if (!is.null(theta)){
  theta = complete_theta(theta)
  
  alpha = theta$alpha
  beta = theta$beta
  gamma = theta$gamma
  delta = theta$delta
}

if (gamma != 0){
  mom.LambertW.U.Gauss = function(gamma) {
    m1 = gamma * exp((gamma^2)/2)
    m2 = exp(2 * gamma^2) * (1 + 4 * gamma^2)
    cm2 = exp(gamma^2) * (exp(gamma^2) * (1 + 4 * gamma^2) - gamma^2)
    m3 = exp((9/2) * gamma^2) * (3 * gamma) * (9 * gamma^2 + 3)
    cm3 = m3 - 3 * m1 * m2 + 2 * m1^3
    m4 = 3 * exp(8 * gamma^2) + 96 * gamma^2 * exp(8 * gamma^2) + 256 * gamma^4 * exp(8 * gamma^2)
    cm4 = m4 - 4 * m1 * m3 + 6 * m1^2 * m2^2 - 3 * m1^4
    skew = cm3/cm2^(3/2)
    kurt = cm4/cm2^2
    list(mu_z = m1, sigma2_z = cm2, skew = skew, kurt = kurt)
  }
  mu_x = beta[1]
  sigma_x = beta[2]
  mom.z = mom.LambertW.U.Gauss(gamma)
  mu_y = mu_x + sigma_x * mom.z$mu_z
  sigma2_y = sigma_x^2 * exp(gamma^2) * ((4 * gamma^2 + 1) * exp(gamma^2) - gamma^2)
  out = list(mean = mu_y, sd = sqrt(sigma2_y), skewness = mom.z$skew, kurtosis = mom.z$kurt)
#        names(out) = c("mu", "sigma", "gamma_1", "gamma_2")
}

if (any(delta != 0)){
  out = list(mean = NA, sd = Inf, skewness = NA, kurtosis = Inf)
  
  moments = function(n=1, delta = delta){
    if (n %% 2) return(0)
    else factorial(n) * (1-n*delta)^(-(n+1)/2)/ (2^(n/2) * factorial(n/2))
  }
  var.delta = function(delta=0){
    moments(n=2, delta=delta)
  }

  kurt = moments(n=4, delta=delta)/var.delta(delta)^2
  names(beta) = NULL
  if (delta < 1) out$mean = beta[1]
  if (delta < 1/2) out$sd = beta[2]*sqrt(var.delta(delta))
  if (delta < 1/3) out$skewness = 0
  if (delta < 1/4) out$kurtosis = kurt
  #out = list(mean = beta[1], sd = beta[2]*sqrt(var.delta(delta)), skewness = 0, kurtosis= kurt)
  #if (delta >= 1) out = list(mean = NA, sd = NA, skewness = NA, kurtosis = NA)
  #if (delta >= 0.5 && delta < 1) out = list(mean = beta[1])
}

if (gamma == 0 && delta == 0) out = list(mean = beta[1], sd = beta[2], skewness = 0 , kurtosis =3)

return(out)
}
