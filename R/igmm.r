IGMM <-
function(y, tol=.Machine$double.eps^0.5, gamma_x = 0,theta.0=c((skewness(y)-gamma_x)/6, median(y), sd(y)), robust=FALSE) UseMethod("IGMM")

