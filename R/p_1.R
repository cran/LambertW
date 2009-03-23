`p_1` <-
function(delta, distname="normal", nu=NULL,...) {
if (delta < 0 ) delta =-delta
if (distname=="normal") {
out=pnorm(-1/delta)
}

if (distname=="t") {

if (nu <=2) stop("Nu <=2. Variance does not exist.")

fac=sqrt(nu/(nu-2))
out=pt(-1/delta*fac, df=nu)
}
out
}

