`summary.LWest` <-
function(object, ...) {
se=sqrt(diag(solve(-object$hessian)))
tval = object$theta/se

TAB = cbind(Esimate = object$theta, StdErr = se, t.value = tval, 
p.value = 2*(1-pnorm(abs(tval))))
dimnames(TAB) = list(names(tval), c(" Estimate",
" Std. Error", " t value", "Pr(>|t|)"))

res=NULL
res$call = object$call
res$coefmat = TAB
res$distname = object$distname
res$nobs = length(object$data)
res$input = get.input(object$data, theta=object$theta)$x
res$support = support(object$theta)
res$data.range = range(object$data)
res$method = object$method
res$p_1 =  p_1(object$theta[1])
if (object$distname=="t") res$p_1 = p_1(object$theta[1], object$distname, nu=object$theta[4])

class(res)= "summary.LWest"
res
}

