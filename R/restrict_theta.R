restrict_theta <-
function(theta, distname = "normal", type = "h", inverse = FALSE){
    if (any(distname == c("normal","cauchy"))) { # only the scale parameter > 0
      if (inverse)  theta$beta[2] = log(theta$beta[2])
      else          theta$beta[2] = exp(theta$beta[2])
      
    }
    if (any(distname == c("gamma","F","chisq", "exp"))) { # all parameters > 0
      if (inverse)  theta$beta = log(theta$beta)
      else          theta$beta = exp(theta$beta)
      
    }
    if (any(type == c("h", "hh"))){
      if (inverse)  theta$delta = log(theta$delta)
      else          theta$delta = exp(theta$delta)
    }
    if (any(type == c("s")) & all(distname != c("normal", "t", "cauchy", "unif"))){
      if (inverse)  theta$gamma = log(theta$gamma)
      else          theta$gamma = exp(theta$gamma)
    }    
    return(theta)
}
