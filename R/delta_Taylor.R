delta_Taylor <-
function(y = NULL, kurtosis_y = kurtosis(y)){
  if (66 * kurtosis_y - 162 > 0) return(max(0, 1/66 * (sqrt(66 * kurtosis_y - 162) - 6)))
  else return(0)
}

