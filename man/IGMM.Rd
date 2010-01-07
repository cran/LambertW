\name{IGMM}
\alias{IGMM}
\alias{IGMM.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Iterative Generalized Method of Moments -- IGMM}
\description{
An iterative method finds this value of \eqn{\theta = (\delta, \mu_x, \sigma_x)} which minimizes the distance between the sample and theoretical skewness of X. For details of the Algorithm see the References.
}
\usage{
IGMM(y, tol = .Machine$double.eps^0.5, gamma_x = 0, theta.0=c((skewness(y)-gamma_x)/6, median(y), sd(y)), robust = FALSE)
\method{IGMM}{default}(y, tol = .Machine$double.eps^0.5, gamma_x = 0, theta.0=c((skewness(y)-gamma_x)/6, median(y), sd(y)), robust = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a numeric vector of real values. }
  \item{tol}{ convergence tolerance (conversion reached); default: \code{.Machine$double.eps^0.5} }
  \item{gamma_x}{ theoretical skewness of input X; default \code{0}}
  \item{theta.0}{ starting values for IGMM algorithm; default: \code{((skewness(y)-gamma_x)/6, median(y), sd(y))}}
  \item{robust}{ robust estimation of the sample skewness (see \code{mc})? default \code{FALSE} }
}

\value{
An object of class \code{LWest}:
  \item{data}{ the data \code{y}}
  \item{theta}{ IGMM estimate for \eqn{\theta} }
  \item{iterations}{number of iterations}
  \item{call}{function call}
  \item{message}{message from the optimization method. What kind of convergence?}
  \item{distname}{ a character string stating the theoretical skewness of the input distribution. Same information as \code{gamma_x}}
  \item{gamma_x}{a-priori imposed theoretical skewness (numeric value); default: \code{0}}
  \item{method}{Estimation method. Here \code{"IGMM"}}
}

\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}

\author{Georg M. Goerg}


\examples{
x=rnorm(1000)
fit=IGMM(x)
summary(fit)

y=rLambertW(n=1000, c(0.1, 2,1))
fity=IGMM(y)
summary(fity)
plot(fity)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{iteration}
\keyword{optimize}
