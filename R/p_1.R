p_1 <-
function(delta, distname="normal", nu=NULL,n=1) {
if (delta < 0 ) delta =-delta

if (distname=="normal") {
out=pnorm(-1/delta)
}

if (distname=="t") {
if (nu <=2) stop("Nu <=2. Variance does not exist.")
fac=sqrt(nu/(nu-2))
out=pt(-1/delta*fac, df=nu)
}

if (!is.character(distname)) out = NA

if (n > 1 & !is.na(out)) {
out = suppressWarnings(pgeom(n-1, prob=out))
if (is.na(out)) out=0
}

out
}
