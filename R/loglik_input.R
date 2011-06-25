loglik_input <-
function(beta = NULL, x = NULL, distname = "normal", f_X = NULL, logf_X = function(x) log(f_X(x))){
  if (distname == "normal") logf_X = function(xx, beta = beta) dnorm(xx, mean = beta[1], sd = beta[2], log = TRUE)
  if (distname == "exp") logf_X = function(xx, beta = beta) dexp(xx, rate = beta[1], log = TRUE)
  if (distname == "cauchy") logf_X = function(xx, beta = beta) dcauchy(xx, location = beta[1], scale = beta[2], log = TRUE)
  if (distname == "chisq") logf_X = function(xx, beta = beta) dchisq(xx, df = beta[1], log = TRUE)
  if (distname == "gamma") {
     if (length(beta) == 2) beta[3] = 1/beta[2]
     logf_X = function(xx, beta = beta) dgamma(xx, shape= beta[1], rate = beta[2], scale = beta[3], log = TRUE)
  }
  if (distname == "unif") logf_X = function(xx, beta = beta) dunif(xx, min = beta[1], max = beta[2], log= TRUE)
  if (distname == "t") logf_X = function(xx, beta = beta) dt((xx-beta[1])/beta[2], df = beta[3], log = TRUE) - log(beta[2])
  if (distname == "user"){
    if (is.null(f_X) && is.null(logf_X)) stop("Please specify either the density function 'f_X = ...' or (preferable) its logarithnm 'logf_X = ...'. \n In the form: 'f_X = function(xx) log(mydensity(xx, params = beta))' , where beta the parameter vector of 'mydensity' and specified as another argument of 'loglik_input'.") 
    #else logf_X = function(xx) logf_X(x = xx)
  }
  
  loglik = sum(logf_X(xx = x, beta = beta))
  return(loglik)
}
