\name{IGMM}
\alias{IGMM}
\alias{IGMM.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Iterative Generalized Method of Moments -- IGMM}
\description{
An iterative method finds this value of theta that results in a sample skewness of x, as close as possible to the theoretical skewness X. Algorithm described in Goerg (2009).
}
\usage{
IGMM(y, robust = FALSE, tol = 10^(-6), gamma_x = 0)
\method{IGMM}{default}(y, robust = FALSE, tol = 10^(-6), gamma_x = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a numeric vector of real values. }
  \item{robust}{ robust estimation of the sample skewness (see \code{mc})? default \code{FALSE} }
  \item{tol}{ tolerance level to stop iteration (conversion reached) }
  \item{gamma_x}{ theoretical skewness of input X; default \code{0}}}

\value{
An object of class \code{LWest}:
  \item{data}{ the data}
  \item{theta}{ IGMM estimate for \eqn{\theta} }
   \item{distname}{ A character string stating the theoretical skewness of the input distribution.}
  \item{iterations}{number of iterations}
  \item{method}{Estimation method. Here \code{"IGMM"}}
  \item{call}{function call}
  \item{gamma_x}{a-priori imposed theoretical skewness}
  \item{message}{message from the optimization method. What kind of convergence?}}
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
