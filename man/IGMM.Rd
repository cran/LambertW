\name{IGMM}
\alias{IGMM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Iterative Generalized Method of Moments -- IGMM}
\description{
Computes via iterations the value of theta that generates appropriate input data x.
}
\usage{
IGMM(y, robust = FALSE, tol = 10^(-6), gamma_x = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ data }
  \item{robust}{ robust measure of the skewness }
  \item{tol}{ tolerance level for conversion reached}
  \item{gamma_x}{theoretical skewness of input X}
  \item{...}{...}
}

\value{
  \item{theta.0}{ the initial estimate \code{theta.0}}
  \item{theta}{optimal theta}
  \item{iterations}{number of iterations}
  \item{call}{...} 
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}


\examples{
x=rnorm(1000)
fit=IGMM(x)
fit
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{iteration}
