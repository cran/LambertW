`plot.LWest` <-
function(x, ...) {

y=x$data

aux.lambert=function(y){
dLambertW(y, theta=x$theta)
}

aux.compare=function(X) {
dnorm(X, mean=mean(y), sd=sd(y))
}

if (x$distname=="t") {
aux.lambert=function(y){
dLambertW(y, theta=x$theta, distname="t")
}

z.t=suppressWarnings(fitdistr(y, "t")$est)

aux.compare=function(X){
nu=z.t[3]
dt((X-z.t[1])/z.t[2], df=nu)/z.t[2]
}
}

x_l=range(y)[1]-0.25*sd(y)
x_u=range(y)[2]+0.25*sd(y)

COL=c(1,2,4)
LWD=c(1,2,1)
LTY=c(1,1,3)

H=hist(y, sqrt(length(y)), plot=FALSE)
D.np=density(y)$y
D.p=aux.compare(seq(x_l, x_u, length=100))
S=support(x$theta)
sup.l=seq(max(S[1], x_l), min(S[2], x_u), length=100)
D.pLW=aux.lambert(sup.l[-c(1, length(sup.l))])

hist(y, sqrt(length(y)), xlim=c(x_l, x_u), main=paste("Density Estimates - ",x$method), 
ylim=range(D.np, D.p, D.pLW, H$intensities), prob=TRUE, density=20, col="darkgray", ylab="")

lines(density(y), lwd=LWD[1], lty=LTY[1])
plot(aux.compare, x_l,x_u,  add=TRUE, lty=LTY[3], col=COL[3], lwd=LWD[3])
plot(aux.lambert,min(sup.l),max(sup.l), lwd=LWD[2], col=COL[2], lty=LTY[2], ylab="",add=TRUE)
abline(v=S, lwd=2, lty=3, col=2)


leg.txt=c("Kernel", "Lambert W - Gaussian", "Gaussian")
if (x$distname=="t") leg.txt=c("Kernel", "Lambert W - t", "t")

pos="topleft"
if (x$theta[1] > 0) pos="topright"
legend(pos, leg.txt, col=COL, lwd=LWD, lty=LTY, cex=0.8) 

pos.r="left"
leg.txt.r=paste("Upper bound: \n b = ", round(S[2],2))
if (x$theta[1] > 0) {
pos.r="right"
leg.txt.r=paste("Lower bound: \n a =",round(S[1],2))
}

legend(pos.r, leg.txt.r)
}

