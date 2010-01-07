normfit <-
function(data, plot.it=TRUE, volatility=FALSE) {
# various normality tests (both graphically and statistically)
# Input: Either a data-vector or a model (will use the residuals of the model)
# Testing vector 'x': normfit(x)
# Testing residuals of model 'mod': normfit(mod)
if (!is.numeric(data)) data=data$res

if (plot.it) {
T=length(data)
h=hist(data, plot=FALSE)
mu=mean(data)
sigma=sd(data)
range.data=c(min(data)-0.1*abs(min(data)),max(data)+0.1*abs(max(data)))
x=seq(range.data[1], range.data[2], length=(2*T))
y=dnorm(x,mu,sigma)

title=""
if (volatility) mfrow=c(3,2)
else mfrow=c(2,2)
par(mfrow=mfrow, mar=c(4.5,4.5,1,1))

plot(data)

#########
hist_dens=function(y) {

aux.compare=function(X) {
dnorm(X, mean=mean(y), sd=sd(y))
}

x_l=range(y)[1]-0.25*abs(range(y)[1])
x_u=range(y)[2]+0.25*abs(range(y)[2])

COL=c(1,2)
LWD=c(1,2)
LTY=c(1,2)

n=length(y)
BS = ceiling((range(y)[2]-range(y)[1])/(3.96 * sd(y)*n^(-1/3)))

H=hist(x=y, breaks=BS, plot=FALSE)
D.np=density(y)$y
D.p=aux.compare(seq(x_l, x_u, length=100))

hist(y, BS, xlim=c(x_l, x_u), ylim=range(D.np, D.p, H$intensities), prob=TRUE, density=10, col="darkgray", main="Empirical/Theoretical Densities", ylab="")
lines(density(y), lwd=LWD[1], main=paste("Density Estimates"))
plot(aux.compare, x_l,x_u,  add=TRUE, lty=LTY[2], col=COL[2], lwd=LWD[2])

if (skewness(y) >= 0) {
legend("topright", c(paste("mean:", round(mean(y), 2)),paste("var: ", round(var(y), 2))) )
legend("right", c("Kernel","Gaussian"), lty=LTY, col=COL, lwd=LWD)
}
if (skewness(y) < 0) {
legend("topleft", c(paste("mean:", round(mean(y), 2)),paste("var: ", round(var(y), 2))) )
legend("left", c("Kernel","Gaussian"), lty=LTY, col=COL, lwd=LWD)
}

}


########
acf(data)
hist_dens(data)
qqnorm(data)
qqline(data)

if (volatility) {
plot(data**2)
acf(data**2)
}
par(mfrow=c(1,1))
}

AD=ad.test(data)
SW=shapiro.test(data)
SF=sf.test(data)
CVM = cvm.test(data)
list(ad=AD, cvm=CVM, sf=SF,sw=SW)
}

