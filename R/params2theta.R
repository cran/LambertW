params2theta <-
function(params, distname="normal", type ="h"){
  theta = list()
  if (any(distname == c("normal","cauchy", "unif", "F", "gamma"))) {
    theta$beta = params[1:2]
  }
  if (any(distname == c("exp","chisq"))) {
    theta$beta = params[1]
  }
  if (distname == "t"){
    theta$beta = params[1:3]
  }
  if (any(type == c("h", "hh"))){
    theta$delta = params[-c(1:length(theta$beta))]
  }
  if (any(type == c("s"))){
    theta$gamma = params[-c(1:length(theta$beta))]
  }
  #theta = complete_theta(theta)
  names(theta$beta) = beta_names(distname = distname)
  return(theta)  
  }

