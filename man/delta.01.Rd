\name{delta.01}
\alias{delta.01}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate input parameters to get a zero mean, unit variance output for a given delta }
\description{
For a given \eqn{\delta} this function computes the input mean \eqn{\mu_x(\delta)} and standard deviation \eqn{\sigma_x(\delta)} such that the resulting Lambert W Gaussian RV has zero mean, and unit variance. This is useful for generating, e.g. a standardized LambertW white noise sequence.

So far only for Gaussian input.
}
\usage{
delta.01(delta, mu_z=0, sigma_z=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{delta}{ skewness parameter }
  \item{mu_z}{ output mean; default \code{0} }
  \item{sigma_z}{ output standard deviation; default \code{1}}
}
\value{
A vector containing the three parameters: (\eqn{\delta},  \eqn{\mu_x(\delta)}, \eqn{\sigma_x(\delta)}).
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}

\examples{
delta.01(0) # for delta = 0, input == output, therefore M=0, S=1
delta.01(0.1) # delta > 0 (positively skewed), thus the input mean must be slightly negative to get a zero-mean output
delta.01(1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
