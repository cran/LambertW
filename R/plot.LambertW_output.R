plot.LambertW_output <-
function(x, a = NULL, b = NULL,...){

obj = x

if (!is.null(obj$parameters$alpha)) alpha = obj$parameters$alpha
else obj$parameters$alpha = 1
if (!is.null(obj$parameters$beta)) beta = obj$parameters$beta
else obj$parameters$beta = obj$beta
if (!is.null(obj$parameters$gamma)) gamma = obj$parameters$gamma
else obj$parameters$gamma = 0
if (!is.null(obj$parameters$delta)) delta = obj$parameters$delta
else obj$parameters$delta = 0

theta = obj$theta

left_lim_0 = FALSE
if (any(obj$input_distname == c("exp","chisq"))) left_lim_0 = TRUE

if (is.null(a)) a = theta[1]-4*theta[2]*(!left_lim_0) + 0.00001
if (is.null(b)) b = theta[1] + (4 + 4*(left_lim_0))*theta[2]

beta_X = obj$parameters$beta
delta = obj$parameters$delta
gamma = obj$parameters$gamma

input_distname_with_beta = substr(obj$distname_with_beta, 13, nchar(obj$distname_with_beta))
input_distname = substr(obj$distname, 13, nchar(obj$distname))

x.seq = seq(a, b, length=101)
parameters_input = obj$parameters
parameters_input$gamma = 0
parameters_input$delta = 0
parameters_input$alpha = 1

y.lim_d = range(c(obj$dY(obj$parameters)(x.seq), obj$dY(parameters_input)(x.seq))) # y-range for density
y.lim_p = range(c(obj$pY(obj$parameters)(x.seq), obj$pY(parameters_input)(x.seq))) # y-range for cdf

if (obj$input_distname == "chisq" && obj$beta == 1) {
y.lim_d = c(0,1)
}

op = par(no.readonly=TRUE)
dev.off()
x11(height=5, width = 9)
par(mfrow=c(1,2), mar=c(4.5,4,3,1))
# pdf plot
plot(obj$dY(obj$parameters),a,b, lwd=2, lty=2, col=2, main = obj$distname_with_beta, ylab="pdf", xlab = "y", ylim=c(max(0, y.lim_d[1]), y.lim_d[2]*1.25))
plot(obj$dY(parameters_input), a, b, add=TRUE, lty=1, col=1, lwd=2)
legend("topright", lwd=2, col=2:1, lty=2:1, cex=1, c(obj$distname_with_beta, input_distname_with_beta))

#mtext(substitute(list(gamma == a, delta == b), list(a=round(theta[3],3), b=round(theta[4],3))))


if (any(x$type == c("s","h"))){
mtext(substitute(list(gamma == b, delta == a), list(a=round(delta,3), b = round(gamma,3))))
}

if (x$type=="hh") {
#mtext(substitute(list(gamma[l] == a, gamma[r] == b), list(a=round(gamma,3)[1], b = round(gamma,3)[2])))
mtext(substitute(list(gamma = a1, delta[l] == a, delta[r] == b), list(a1 = round(gamma,3), a=round(delta,3)[1], b = round(delta,3)[2])))
}

if (length(delta) == 2 && length(gamma) == 2) {
#mtext(substitute(list(gamma[l] == a, gamma[r] == b), list(a=round(gamma,3)[1], b = round(gamma,3)[2])))
mtext(substitute(list(gamma[l] == a1, gamma[r] == b1, delta[l] == a, delta[r] == b), list(a1 = round(gamma,3)[1], b1 = round(gamma,3)[2], a=round(delta,3)[1], b = round(delta,3)[2])))
}


# cdf plot
plot(obj$pY(obj$parameters),a,b, lwd=2,lty=2, col=2, main = obj$distname_with_beta, ylab="cdf", xlab = "y", ylim=c(max(0, y.lim_p[1]), y.lim_p[2]*1.25))
plot(obj$pY(parameters_input), a, b, add=TRUE, lty=1, col=1, lwd=2)
abline(h=c(0,1))
legend("topleft", lwd=2, col=2:1, lty=2:1, cex=1, c(obj$distname_with_beta, input_distname_with_beta))

#if (length(delta) == 1 && length(gamma) == 1) {
if (any(x$type == c("s","h"))){
mtext(substitute(list(gamma == b, delta == a), list(a=round(delta,3), b = round(gamma,3))))
}

#if (length(delta) == 2 && length(gamma) == 1) {
if (x$type=="hh") {
#mtext(substitute(list(gamma[l] == a, gamma[r] == b), list(a=round(gamma,3)[1], b = round(gamma,3)[2])))
mtext(substitute(list(gamma = a1, delta[l] == a, delta[r] == b), list(a1 = round(gamma,3), a=round(delta,3)[1], b = round(delta,3)[2])))
}

if (length(delta) == 2 && length(gamma) == 2) {
#mtext(substitute(list(gamma[l] == a, gamma[r] == b), list(a=round(gamma,3)[1], b = round(gamma,3)[2])))
mtext(substitute(list(gamma[l] == a1, gamma[r] == b1, delta[l] == a, delta[r] == b), list(a1 = round(gamma,3)[1], b1 = round(gamma,3)[2], a=round(delta,3)[1], b = round(delta,3)[2])))
}

#legend("bottomright", c(as.expression(bquote(gamma == .(round(obj$parameters$gamma,3)))),
#as.expression(bquote(delta == .(round(obj$parameters$delta,3))))))
par(op)

}

