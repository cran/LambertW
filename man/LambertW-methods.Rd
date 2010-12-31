\name{LambertW-methods}
\alias{dLambertW}
\alias{pLambertW}
\alias{qLambertW}
\alias{rLambertW}
\alias{mLambertW}
\alias{qqLambertW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The Lambert W x F Distribution}
\description{
Density, distribution function, quantile function and random number generation for the Lambert W \eqn{\times} \eqn{F_X(x \mid \boldsymbol \beta)} distribution with parameter vector \eqn{(\alpha, \boldsymbol \beta, \gamma, \delta)}.

Implemented distributions (\code{distname = ...}) are:  \code{"cauchy"}, \code{"chisq"}, \code{"exp"}, \code{"normal"}, \code{"t"}, \code{"unif"}. To be extended.

\code{qqLambertW} computes and plots the sample quantiles of the data \code{y} versus the theoretical Lambert W \eqn{\times} \eqn{F} theoretical quantiles implied by the parameter estimate \eqn{\widehat{\theta}}.

\code{mLambertW} computes the first 4 central moments of a Lambert W \eqn{\times} Gaussian.
}
\usage{
dLambertW(y, beta = c(0,1), gamma = 0, delta = 0, alpha = 1, distname = c("normal"), input.U = NULL)
pLambertW(q, beta = c(0,1), gamma = 0, delta = 0, alpha = 1, distname = c("normal"), input.U = NULL) 
qLambertW(p, beta = c(0,1), gamma = 0, delta = 0, alpha = 1, distname = c("normal"), input.U = NULL)
rLambertW(n = 1000, beta = c(0,1), gamma = 0, delta = 0, alpha = 1, distname = "normal", return.input = FALSE, input.U = NULL)
mLambertW(beta = c(0, 1), distname = c("normal"), gamma = 0, delta = 0, alpha = 1)
qqLambertW(y, beta = c(0,1), gamma = 0, delta = 0, alpha = 1, distname = c("normal"), plot.it = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y,q}{ vector of quantiles. }
  \item{p}{ vector of probability levels }
  \item{n}{ number of observations}
  \item{distname}{input distribution. default \code{"normal"}.}
  \item{beta}{ parameter vector \eqn{\boldsymbol \beta}  of the input distribution}
  \item{gamma}{ skewness parameter; default: \code{0} }
  \item{delta}{ heavy-tail parameter (= Tukey's h in case of a normal distribution); default: \code{0} }
  \item{alpha}{ exponent; default: \code{1} }
\item{return.input}{ indicator for returning the simulated input x that gets transformed to the actual observations \code{y}; default: \code{FALSE}. Warning: if \code{TRUE} then the output is not a vector anymore, but a list containing the input \code{x} and the output \code{y}.}
  \item{plot.it}{ Should the QQ plot be displayed or only the (sorted) theoretical and empirical quantiles be returned? default: \code{TRUE}.}
  \item{input.U}{ you can supply your own version of U (either a vector of simulated values or a function defining the pdf/cdf/quanitle function of U); default: \code{NULL}.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
See the references for the analytic expressions of the pdf and cdf. Quantiles are computed numerically for the \code{type = "s"} case (no analytic expression yet); for \code{"h"} or \code{"hh"} quantiles can be computed analytically given the input quantile function and \eqn{\theta}.

All functions have the optinal argument of user-supplied versions of the zero-mean, unit variance input U. For example, if you have your own density function as \code{dmydist(u) = function(u) ...)} then you can pass this function to \code{dLambertW(..., input.U = dmydist(u))} and it will use this pdf to generate plots, estimates, etc.  Important: for user-defined input, all \code{input.U} in \code{dLambertW}, \code{pLambertW}, ... must be supplied correctly. 
}

\value{
Following the standard distributions in R (e.g. \code{rnorm}, \code{rexp}, ...) the notation and functionality of LambertW distribution work as expected:
\code{dLambertW}  computes the value of the pdf at \code{y}, \code{pLambertW} the cdf at \code{y}, \code{qLambertW} is the quantile function, and \code{rLambertW} generates random deviates from a Lambert W \eqn{\times} \eqn{F_X(x \mid \boldsymbol \beta)} distribution.

\code{mLambertW} returns a vector with the 4 theoretical (central/standardized) moments of \eqn{Y} implied by \eqn{\boldsymbol \beta} and \code{distname} (works so far only for \code{distname = "normal"}):
  \item{mu}{mean}
  \item{sigma}{standard deviation}
  \item{skewness}{skewness}
  \item{kurtosis}{kurtosis (i.e. = 3 for a Gaussian)}

\code{qqLambertW} returns a list of 2 vectors (in accordance to the output of \code{qqnorm}):
  \item{x}{theoretical quantiles (sorted)}
  \item{y}{empirical quantiles (sorted)}

By default \code{rLambertW} returns a sampled Lambert W \eqn{\times} \eqn{F_X(x \mid \boldsymbol \beta)} data vector. If \code{return.input = TRUE}, then the output is a list of 2 vectors:
  \item{x}{simulated input}
  \item{y}{Lambert W random sample (given by \code{x} and the functional relation \eqn{y = func(x)} - see References.}

}
\references{Goerg, G.M. (2011a). \dQuote{Lambert W Random Variables - A New Family of Generalized Skewed Distributions with Applications to Risk Estimation}. In review at the AOAS (arxiv.org/abs/0912.4554).

Goerg, G.M. (2011b). \dQuote{The Lambert Way to Gaussianize skewed, heavy-tailed data with the inverse of Tukey's h transformation as a special case.}. In preparation for submission (http://arxiv.org/abs/1010.2265).
}
\author{Georg M. Goerg}

\examples{
###############################
######### mLambertW ###########
mLambertW(beta = c(0,1), gamma = 0.1)
mLambertW(gamma=0.1) # the same as by default: mean=0, standard deviation =1
mLambertW(beta = c(1,1), gamma = 0.1) # mean shifted by 1
mLambertW(gamma=0) # N(0,1)

###############################
######### rLambertW ###########
set.seed(1)
x=rLambertW(n=1000, beta=c(0,1)) # same as rnorm(1000)
skewness(x) # very small skewness
mc(x) # also close to zero

y = rLambertW(n=1000, beta=c(1,3), gamma = 0.1)
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
beta.s = c(0,1)
gamma.s = 0.1

aux.dlambert=function(y){
dLambertW(y, beta=beta.s, gamma = gamma.s, distname="normal")
}

x11(width=10, height=5)
par(mfrow=c(1,2), mar=c(3,3,3,1))
plot(aux.dlambert, -3.5,5, ylab="", main="Density function")
plot(dnorm, -3.5,5, add=TRUE, lty=2)
legend("topright" , c("Lambert W - Gaussian" , "Gaussian"), lty=1:2)
abline(h=0)

###############################
######### pLambertW ###########

aux.plambert=function(y){
pLambertW(y, beta=beta.s, gamma = gamma.s, distname="normal")
}

plot(aux.plambert, -3.5,3.5, ylab="", main="Distribution function")
plot(pnorm, -3.5,3.5, add=TRUE, lty=2)
legend("topleft" , c("Lambert W x Gaussian" , "Gaussian"), lty=1:2)
par(op)

######## Animation 
gamma.v = seq(-0.15,0.15, length=31) # typical, empirical range of gamma
b = support(gamma_01(min(gamma.v)))[2]*1.1
a = support(gamma_01(max(gamma.v)))[1]*1.1

for (ii in 1:length(gamma.v)) {
aux.dlambert=function(y){
dLambertW(y, beta = gamma_01(gamma.v[ii])[1:2], gamma = gamma.v[ii], distname="normal") # zero-mean, unit variance for all gamma
#dLambertW(y, beta = c(0,1), gamma = gamma.v[ii], distname="normal") # mean and variance change with gamma; only for gamma=0: mean=0, sd=1
}

plot(aux.dlambert, a,b, ylab="", lty=2, col=2, lwd=2, main="pdf", ylim = c(0,0.45))
plot(dnorm, a,b, add=TRUE, lty=1, lwd=2)
legend("topright" , c("Lambert W x Gaussian" , "Gaussian"), lty=2:1, lwd=2, col=2:1)
abline(h=0)
legend("topleft", cex=1.3, c(as.expression(bquote(gamma == .(round(gamma.v[ii],3))))))
Sys.sleep(0.05)
}

###############################
######### qLambertW ###########

p.v=c(0.01, 0.05, 0.5, 0.9, 0.95,0.99)

qnorm(p.v)
qLambertW(p.v, beta = c(0,1), distname="normal", gamma = 0, delta = 0, alpha = 1) # the same as above except for numerical rounding errors; will be improved in future versions.

# positively skewed data -> quantiles are higher
qLambertW(p.v, beta = c(0,1), distname="normal", gamma = 0.1, delta = 0, alpha = 1)

###############################
######### qqLambertW ###########

y=rLambertW(n=500, distname="normal", beta = c(0,1), gamma = 0.1)
x11(height=6, width=9)
par(mfrow=c(1,2), mar=c(4,4,3,1))
qqnorm(y)
qqline(y)
qqLambertW(y, beta = c(0,1), gamma = 0.1)
par(op)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
\keyword{distribution}
\keyword{datagen}
