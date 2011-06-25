gamma_Taylor <-
function(y = NULL, skewness_y = skewness(y), skewness_x = 0){
  gamma.hat = (skewness_y - skewness_x)/6
  return(sign(gamma.hat)*min(abs(gamma.hat), 0.25))
}
