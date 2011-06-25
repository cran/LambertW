delta_Taylor <-
function(y = NULL, kurtosis_y = kurtosis(y)){
  if (66 * kurtosis_y - 162 > 0) delta.hat = max(0, 1/66 * (sqrt(66 * kurtosis_y - 162) - 6))
  else delta.hat = 0
  return(min(delta.hat, 1/4))
}
