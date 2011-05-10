p_1 <-
function(gamma = 0, beta = c(0,1), distname="normal", n = 1) {
if (gamma < 0 ) gamma =-gamma
out = pU(-1/gamma, beta = beta, distname=distname)
names(out) = NULL

if (n > 1 & !is.na(out)) {
out = suppressWarnings(pgeom(n-1, prob=out))
if (is.na(out)) out=0
}
out
}

