plot.LambertW_input <-
function(x, a = NULL, b = NULL,...){

obj = x
theta = obj$theta

left_lim_0 = FALSE
if (any(obj$distname == c("exp","chisq"))) left_lim_0 = TRUE

if (is.null(a)) a = theta[1]-3*theta[2]*(!left_lim_0) + 0.00001
if (is.null(b)) b = theta[1] + (3 + 4*(left_lim_0))*theta[2]

input_distname_with_beta = obj$distname_with_beta
input_distname = obj$distname

op = par(no.readonly=TRUE)
dev.off()
x11(height=4, width = 9)
par(mfrow=c(1,2), mar=c(4,4,3,1))
# pdf plot
plot(obj$dX,a,b, lwd=2, main = obj$distname_with_beta, ylab="pdf", xlab = "x")
# cdf plot
plot(obj$pX,a,b, lwd=2, main = obj$distname_with_beta, ylab="cdf", xlab = "x")
par(op)

}

