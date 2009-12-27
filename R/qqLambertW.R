qqLambertW <-
function(y, theta=IGMM(y)$theta, distname=c("normal"), plot.it = TRUE, ...) 
{
if (length(theta) == 3 & distname=="t") stop("You must specify a degrees of freedom parameter for student-t input.")

xlab = "Theoretical Quantiles"
ylab = "Sample Quantiles"

nu=theta[4]
delta=theta[1]
main = "Lambert W - Gaussian Q-Q Plot"
if (distname=="t") {
main = "Lambert W - t Q-Q Plot"
}

    y <- y[!is.na(y)]
    if (0 == (n <- length(y))) 
        stop("y is empty")
 p.n=ppoints(n)
 x=qLambertW(p.n, theta, distname)
    if (plot.it) {
   plot(sort(x), sort(y), main = main, xlab = xlab, ylab = ylab, ...)
   abline(0,1)
}
    invisible(list(x = sort(x), y = sort(y)))
}

