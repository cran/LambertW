\name{LambertW-methods}
\alias{dLambertW}
\alias{pLambertW}
\alias{qLambertW}
\alias{rLambertW}
\alias{qqLambertW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cumulative Distribution Function}
\description{
Density, distribution function, quantile function and random generation for the Lambert W \eqn{\times} Gaussian (or student-t) distribution with parameter vector \eqn{\theta = (\delta, \mu_x, \sigma_x)} for Gaussian (or \eqn{\theta = (\delta, \mu_x, \sigma_x, \nu)} for student-t) input.

Also a QQ-plot for LambertW - Gaussian (or t) of the data values is available.
}
\usage{
dLambertW(y, theta = c(0, 0, 1), distname = c("normal"))
pLambertW(q, theta = c(0, 0, 1), distname = c("normal"))
qLambertW(p, theta = c(0, 0, 1), distname = c("normal"))
rLambertW(n, theta = c(0, 0, 1), distname = c("normal"), input = NULL, ZmUv = FALSE, return.input = FALSE)

qqLambertW(y, theta = IGMM(y)$theta, distname = c("normal"), plot.it = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y,q}{ vector of quantiles. }
  \item{p}{ vector of probability levels }
  \item{n}{ number of observations}
  \item{theta}{ parameter vector }
  \item{distname}{input distribution. default \code{"normal"}. alternative \code{"t"}}
  \item{input}{pre-specified input data; optional.}
  \item{ZmUv}{ Should the mean and standard deviation of X be chosen such that Y is a zero mean, unit variance RV? default \code{FALSE}}
  \item{return.input}{ Should the simulated input x, leading to output y be returned? default \code{FALSE}; Caution: if \code{TRUE} then the output is not a vector anymore, but a list containing the input x and the output y.}
  \item{plot.it}{ should it be plotted }
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
See the references for the expressions of the pdf and cdf. Quantile function is computed numerically (no analytic expression yet). 

Random number generation allows to specify input data, and also has the option of zero mean and unit variance output. If \code{ZmUv = TRUE}, then the mean and variance of the input are adjusted. This is useful e.g. in (financial) time series analysis, where often an iid white noise sequence with these properties is necessary.
Caution: if \code{return.input = TRUE} then the output is not a vector anymore, but a list containing the input x and the output y.
}

\value{
"dLambertW" gives the density, "pLambertW" gives the distribution
     function, "qLambertW" gives the quantile function, and "rLambertW"
     generates random deviates.

\code{qqLambertW} returns a list of 2 qantiles
  \item{x}{Theoretical quantiles (sorted)}
  \item{y}{Empirical quantiles (sorted)}

if \code{rLambertW} has \code{return.input = TRUE} then the output is a list of 2 vectors
  \item{x}{simulated input}
  \item{y}{simulated Lambert W output}
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}

\examples{
theta.s=c(0.1,0,1)

set.seed(1)
## rLambertW ##
x=rnorm(1000)
skewness(x) # very small skewness
mc(x) # close to zero

y=rLambertW(n=1000, theta.s)
mom.LambertW.X.Gauss(theta.s)
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
## dLambertW
aux.dlambert=function(y){
dLambertW(y, theta=theta.s)
}

support(theta.s)

plot(aux.dlambert, -3.5,5, ylab="Probability")
plot(dnorm, -3.5,5, add=TRUE, lty=2)
legend("topright" , c("Lambert W - Gaussian" , "Gaussian"), lty=1:2)
abline(h=0)

## pLambertW ##

aux.plambert=function(y){
pLambertW(y, theta=theta.s)
}

support(theta.s)

plot(aux.plambert, -3.5,3.5, ylab="Probability")
plot(pnorm, -3.5,3.5, add=TRUE, lty=2)
legend("topleft" , c("Lambert W - Gaussian" , "Gaussian"), lty=1:2)

## qLambertW
p.v=c(0.01, 0.05, 0.5, 0.9, 0.95,0.99)

qnorm(p.v)
sapply(p.v, qLambertW, theta=c(0,0,1)) # should give the same as above

# positively skewed data -> quantiles are higher
qLambertW(p.v, theta=theta.s)

## qqLambertW
y=rLambertW(n=200, theta.s)
qqnorm(y)
qqline(y)
qqLambertW(y, theta=IGMM(y)$theta)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
