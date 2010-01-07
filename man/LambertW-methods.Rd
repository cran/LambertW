\name{LambertW-methods}
\alias{dLambertW}
\alias{pLambertW}
\alias{qLambertW}
\alias{rLambertW}
\alias{mLambertW}
\alias{qqLambertW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cumulative Distribution Function}
\description{
Density, distribution function, quantile function and random generation for the Lambert W \eqn{\times} Gaussian (or student-t) distribution with parameter vector \eqn{\theta = (\delta, \mu_x, \sigma_x)} for Gaussian (or \eqn{\theta = (\delta, \mu_x, \sigma_x, \nu)} for student-t) input.

A QQ-plot for LambertW - Gaussian (or t) for given data can be displayed.
Computation of theoretical moments. So far only for Lambert W - Gaussian, i.e\ \eqn{\theta = (\delta, \mu_x, \sigma_x)}.
}
\usage{
dLambertW(y, theta = c(0, 0, 1), distname = c("normal"))
pLambertW(q, theta = c(0, 0, 1), distname = c("normal"))
qLambertW(p, theta = c(0, 0, 1), distname = c("normal"))
rLambertW(n, theta = c(0, 0, 1), distname = c("normal"), input = NULL, ZmUv = FALSE, return.input = FALSE)

mLambertW(delta = 0, theta = c(delta, 0,1), distname = c("normal"))
qqLambertW(y, theta = IGMM(y)$theta, distname = c("normal"), plot.it = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y,q}{ vector of quantiles. }
  \item{p}{ vector of probability levels }
  \item{n}{ number of observations}
  \item{theta}{ parameter vector }
  \item{delta}{ skewness parameter }
  \item{distname}{input distribution. default \code{"normal"}. alternative \code{"t"}}
  \item{input}{pre-specified input data; optional.}
  \item{ZmUv}{ Should the mean and standard deviation of X be chosen such that Y is a zero mean, unit variance RV? default \code{FALSE}. See \code{\link{delta.01}}.}
  \item{return.input}{ Should the simulated input x, leading to output y be returned? default \code{FALSE}; Caution: if \code{TRUE} then the output is not a vector anymore, but a list containing the input x and the output y.}
  \item{plot.it}{ Should the QQ plot be displayed or only the (sorted) theoretical and empirical quantiles be returned? default: \code{TRUE}. }
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
See the references for the analytic expressions of the pdf and cdf. Quantile function is computed numerically (no analytic expression yet). 

Random number generation allows to specify input data, and also has an option (\code{ZmUv = TRUE}) to force (theoretically) \eqn{\mu_y = 0} and \eqn{\sigma_y =1}. This is particularly useful in (financial) time series analysis, where often an iid white noise sequence with these properties is necessary.

Caution: if \code{return.input = TRUE} then the output is not a vector anymore, but a list containing the input x and the output y.
}

\value{
\code{dLambertW}  gives the density, \code{pLambertW} the cumulative distribution
     function, \code{qLambertW} the quantile function, and \code{rLambertW} 
     generates random deviates.

\code{mLambertW} returns a vector with the 4 theoretical (central / standardized) moments of \eqn{Y} implied by \eqn{\delta} and/or \eqn{\theta} (works so far only for \code{distname = "normal"}):
  \item{mu}{mean}
  \item{sigma}{standard deviation}
  \item{gamma_1}{skewness}
  \item{gamma_2}{excess kurtosis (=0 for Gaussian)}

\code{qqLambertW} returns a list of 2 vectors: (in accordance to the output of \code{qqnorm})
  \item{x}{theoretical quantiles (sorted)}
  \item{y}{empirical quantiles (sorted)}

By default \code{rLambertW} returns the sampled LambertW - F data vector. If \code{return.input = TRUE}, then the output is a list of 2 vectors:
  \item{x}{simulated input}
  \item{y}{Lambert W random sample (implied by \code{x})}

}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}

\examples{
theta.s=c(0.1,0,1)
mLambertW(theta.s)
mLambertW(delta=0.1) # the same as by default: mean=0, standard deviation =1
mLambertW(theta=c(0.1, 1,1)) # mean shifted by 1
mLambertW(delta=0) # N(0,1)

set.seed(1)
x=rnorm(1000)
skewness(x) # very small skewness
mc(x) # also close to zero

###############################
######### rLambertW ###########
y=rLambertW(n=1000, theta.s)
mLambertW(theta.s)
skewness(y) # high positive skewness (in theory equal to 3.70)
mc(y) # also the robust measure gives a high value


op=par(no.readonly=TRUE)
par(mfrow=c(2,2), mar=c(2,4,3,1))
plot(x)
hist(x, prob=TRUE, 15)
lines(density(x))

plot(y)
hist(y, prob=TRUE, 15)
lines(density(y))
par(op)
###############################
######### dLambertW ###########

aux.dlambert=function(y){
dLambertW(y, theta=theta.s)
}

support(theta.s)

x11(width=10, height=5)
par(mfrow=c(1,2), mar=c(2,2,2,1))
plot(aux.dlambert, -3.5,5, ylab="", main="Density function")
plot(dnorm, -3.5,5, add=TRUE, lty=2)
legend("topright" , c("Lambert W - Gaussian" , "Gaussian"), lty=1:2)
abline(h=0)

###############################
######### pLambertW ###########

aux.plambert=function(y){
pLambertW(y, theta=theta.s)
}

plot(aux.plambert, -3.5,3.5, ylab="", main="Distribution function")
plot(pnorm, -3.5,3.5, add=TRUE, lty=2)
legend("topleft" , c("Lambert W - Gaussian" , "Gaussian"), lty=1:2)
par(op)

######## Animation 
delta.v = seq(-0.15,0.15, length=101) # typical, empirical range of delta
b = support(delta.01(min(delta.v)))[2]
a = support(delta.01(max(delta.v)))[1]

for (i in 1:length(delta.v)) {
aux.dlambert=function(y){
#dLambertW(y, theta=delta.01(delta.v[i])) # always zero-mean, unit variance for any delta
dLambertW(y, theta=c(delta.v[i],0,1)) # mean, and variance change with delta; only for delta=0: mean =0, sd=1
}

plot(aux.dlambert, a,b, ylab="", main="Density function")
plot(dnorm, a,b, add=TRUE, lty=2)
legend("topright" , c("Lambert W - Gaussian" , "Gaussian"), lty=1:2)
abline(h=0)
legend("bottom", paste("delta = ", delta.v[i]))

Sys.sleep(0.1)
}

###############################
######### qLambertW ###########

p.v=c(0.01, 0.05, 0.5, 0.9, 0.95,0.99)

qnorm(p.v)
qLambertW(p.v, theta=c(0,0,1)) # the same as above (numerical inacurracy; improved in next version)

# positively skewed data -> quantiles are higher
qLambertW(p.v, theta=theta.s)

###############################
######### qqLambertW ###########

y=rLambertW(n=500, theta.s)
x11(height=5, width=10)
par(mfrow=c(1,2), mar=c(4,4,3,1))
qqnorm(y)
qqline(y)
qqLambertW(y, theta=IGMM(y)$theta)
par(op)
###############################
######### mLambertW ###########

mLambertW(theta.s)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
