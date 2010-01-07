summary.LWest <-
function(object, ...) {

if (!is.null(object$hessian)) se=sqrt(diag(solve(object$hessian)))

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
res$data = object$data
res$input = get.input(object$data, theta=object$theta)
res$coefmat = TAB
res$hessian=object$hessian
res$distname = object$distname
res$n = length(object$data)
res$support = support(object$theta)
res$data.range = range(object$data)
if (object$method == "MLE") {
res$p_1 = p_1(delta=object$theta[1], distname=object$distname, nu=object$theta[4], n=1)
res$p_1n = p_1(delta=object$theta[1], distname=object$distname, nu=object$theta[4], n=res$n)
}
else res$p_1 = res$p_1n = NA
class(res)= "summary.LWest"
res
}

