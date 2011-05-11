\name{U-methods}
\alias{dU}
\alias{rU}
\alias{pU}
\alias{qU}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The zero-mean, unit-variance version of standard distributions}
\description{
Density, distribution function, quantile function and random number generation for the (zero-mean and) unit variance version U of the (location-)scale family input  \eqn{X \sim F_X(x \mid \boldsymbol \beta)} - see References.
}
\usage{
dU(u, beta = NULL, distname=c("normal"))
rU(n = NULL, beta = NULL, distname=c("normal"))
pU(u, beta = NULL, distname = c("normal"))
qU(p, beta = NULL, distname = c("normal")) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{u}{ vector of quantiles. }
  \item{n}{ number of observations}
  \item{p}{ vector of probability levels }
  \item{beta}{ parameter vector of input distribution \eqn{F_X} (appropriate centering/scaling is done internally).}
  \item{distname}{input distribution. default \code{"normal"}.}
}
\details{
Since the zero-mean, unit-variance (or only unit-variance) version U is one of the main building blocks of Lambert W \eqn{\times} F distributions, these functions are wrappers to be used by other functions such as \code{\link{dLambertW}} or \code{\link{rLambertW}}.
}

\value{
\code{dU} evaluates the pdf at \code{y}, \code{pU} evaluates the cdf, \code{qU} is the quantile function, and \code{rU} generates random deviates of U.
}

\references{
Goerg, G.M. (2011a). \dQuote{Lambert W Random Variables - A New Family of Generalized Skewed Distributions with Applications to Risk Estimation}. Forthcoming in the Annals of Applied Statistics (http://arxiv.org/abs/0912.4554).
}
\author{Georg M. Goerg}

\examples{
# a zero-mean, unit variance version of the t_3 distribution.
plot(seq(-4, 4, length = 100),dU(seq(-4, 4, length = 100), beta = c(1,1,3), distname = "t"), type="l")
# the cdf of the unit-variance version of an exp(3) -> just an exp(1)
plot(seq(0, 4, length = 100),pU(seq(0, 4, length = 100), beta = 3, distname = "exp"), type="l") 

# all have variance 1
var(rU(n = 1000, distname = "chisq", beta = 2))
var(rU(n = 1000, distname = "normal", beta = c(3,3)))
var(rU(n = 1000, distname = "exp", beta = 1))
var(rU(n=1000, distname="unif", beta = c(0,10)))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
\keyword{distribution}
\keyword{datagen}
