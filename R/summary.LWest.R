`summary.LWest` <-
function(object, ...) {

if (!is.null(object$logLH)) {
H=numericNHessian(f=object$logLH, t0=object$theta)
object$hessian=H
se=sqrt(diag(solve(-object$hessian)))
}

if (object$method=="IGMM") {
se=c(0.4, 1, sqrt(1/2))/sqrt(length(object$data))
}

tval = object$theta/se

TAB = cbind(Esimate = object$theta, StdErr = se, t.value = tval, 
p.value = 2*(1-pnorm(abs(tval))))
dimnames(TAB) = list(names(tval), c(" Estimate", " Std. Error", " t value", "Pr(>|t|)"))

res=NULL
res$call = object$call
res$method = object$method
res$input = get.input(object$data, theta=object$theta)$x
res$coefmat = TAB
res$hessian=object$hessian
res$distname = object$distname
res$nobs = length(object$data)
res$support = support(object$theta)
res$data.range = range(object$data)
res$p_1 =  p_1(object$theta[1])
if (object$distname=="t") res$p_1 = p_1(object$theta[1], object$distname, nu=object$theta[4])

class(res)= "summary.LWest"
res
}

