plot.LambertW_fit <-
function(x, QQ = FALSE, a = NULL, b = NULL, ...) {

xlim_user = c(a,b)

y=x$data

tau_all = rep(0, 5)
tau_all[1:2] = x$tau[1:2]
tau_all[5] = 1

if (x$type == "s") {
	gamma_hat = x$theta$gamma
	tau_all[3] = gamma_hat
	}
if (any(x$type == c("h", "hh"))) {
	delta_hat = x$theta["delta"]
	tau_all[4] = delta_hat
	}
if (x$method == "IGMM") x$distname = "normal"
if (x$distname == "normal") x$beta = x$tau[1:2]

#xx_input = get.input(y, theta = tau_all)

aux.lambert=function(xx){
if (x$type == "s") return(dLambertW(xx, beta = x$beta, gamma = x$theta$gamma, distname = x$distname))
if (any(x$type == c("h", "hh"))) return(dLambertW(xx, beta = x$beta, delta = x$theta$delta, distname = x$distname))
}

aux.compare = function(xx){
if (x$distname == "exp") beta_y = suppressWarnings(fitdistr(y, x$distname)$est)
else if (x$distname == "chisq") beta_y = mean(y)
else beta_y = suppressWarnings(fitdistr(y, x$distname)$est)

return(dLambertW(xx, beta = beta_y, gamma = 0,delta = 0, alpha = 1, distname = x$distname))
}

x_l=range(y)[1]-0.2*sd(y)
x_u=range(y)[2]+0.2*sd(y)

if (!is.null(xlim_user[1])) x_l = xlim_user[1]
if (!is.null(xlim_user[2])) x_u = xlim_user[2]

COL=c(1,2,4) #Kernel, LambertW, Original
LWD=c(2,2,1)
LTY=c(1,1,2)

n=length(y)
BS = ceiling((range(y)[2]-range(y)[1])/(3.96 * sd(y)*n^(-1/3)))
#y.window = y[y>a && y<b]
#BS = ceiling((range(y.window)[2]-range(y.window)[1])/(3.96 * sd(y.window)*n^(-1/3)))

H=hist(y, BS, plot=FALSE)
D.np=density(y)$y
D.p=aux.compare(seq(x_l, x_u, length=100))
S = c(-Inf, Inf)
if (x$type == "s") S=support(x$tau)

sup.l=seq(max(S[1], x_l), min(S[2], x_u), length=100)
D.pLW=aux.lambert(sup.l[-c(1, length(sup.l))])

hist(y, BS, xlim=c(x_l, x_u), main=paste("Density Estimates"), 
ylim=range(D.np, D.p, D.pLW, H$intensities), prob=TRUE, density=20, col="darkgray", ylab="")

lines(density(y), lwd=LWD[1], lty=LTY[1])
plot(aux.compare, x_l,x_u,  add=TRUE, lty=LTY[3], col=COL[3], lwd=LWD[3])
plot(aux.lambert,min(sup.l),max(sup.l), lwd=LWD[2], col=COL[2], lty=LTY[2], ylab="",add=TRUE)
abline(v=S, lwd=2, lty=3, col=2)

leg.txt=c("Kernel", paste("Lambert W - ",x$distname,"\n (", x$method,"; type: '",x$type ,"')", sep=""), x$distname)
#if (x$distname=="t") leg.txt=c("Kernel", "Lambert W - t", "t")

pos="topleft"

if (x$type == "s"){
 	if (gamma_hat > 0) pos="topright"
}
legend(pos, leg.txt, col=COL, lwd=LWD, lty=LTY, cex=0.8) 

if (x$type == "s"){
	pos.r="left"
	leg.txt.r=paste("Upper bound: \n b = ", round(S[2],2))
	if (gamma_hat > 0) {
		pos.r="right"
		leg.txt.r=paste("Lower bound: \n a =",round(S[1],2))
	}
	#legend("bottomright", c(as.expression(bquote(gamma == .(round(obj$theta$gamma,3)))),
	#leg.txt.r = c(as.expression(bquote(gamma == .(round(x$theta$gamma,3)))), leg.txt.r)
	legend(pos.r, leg.txt.r)
}

if (QQ) {
	x11()
	if (x$type == "s") qqLambertW(y, beta = x$beta, gamma = x$theta$gamma, distname=x$distname)
	if (any(x$type == c("h", "hh"))) qqLambertW(y, beta = x$beta, delta = x$theta$delta, distname=x$distname)
}
# end of plotting function
}

