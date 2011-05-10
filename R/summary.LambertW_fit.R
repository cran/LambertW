summary.LambertW_fit <-
function(object, ...) {
  if (object$method == "IGMM") object$param.hat = object$tau
  
  se=suppressWarnings(sqrt(-diag(solve(object$hessian))))
  
  tval = object$param.hat/se
  
  TAB = cbind(Esimate = object$param.hat, StdErr = se, t.value = tval, 
  p.value = 2*(1-pnorm(abs(tval))))
  dimnames(TAB) = list(names(tval), c(" Estimate", " Std. Error", " t value", "Pr(>|t|)"))
  
  res=NULL
  res$call = object$call
  res$method = object$method
  res$data = object$data
  res$input = get.input(object$data, tau=object$tau)
  res$coefmat = TAB
  res$hessian=object$hessian
  res$distname = object$distname
  res$type = object$type
  res$n = length(object$data)
  res$support = support(object$tau)
  res$data.range = range(object$data)
  if (object$method == "MLE" && object$type == "s") {
  res$p_1 = p_1(gamma=object$theta$gamma, distname=object$distname, beta=object$theta$beta, n=1)
  res$p_1n = p_1(gamma=object$theta$gamma, distname=object$distname, beta=object$theta$beta, n=res$n)
  }
  else res$p_1 = res$p_1n = NA
  if (object$type == "hh"){
  res$symmetry_pval = skewness_test(object)$p.value
  }
  class(res)= "summary.LambertW_fit"
  res
}

