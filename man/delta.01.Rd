\name{delta.01}
\alias{delta.01}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate delta for zero mean, unit variance output}
\description{
This function calculates the input mean and standard deviation given \eqn{\delta} such that the resulting Lambert W Gaussian RV has zero mean, and unit variance. This is useful for generating, e.g. a standardized white noise sequence.
}
\usage{
delta.01(delta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{delta}{ skewness parameter \code{delta} }
}

\value{
  \item{M }{mean of X}
  \item{S }{standard deviation of X}
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}


\examples{
delta.01(0) # for delta = 0 it holds input == output, therefore M =0, S=1
delta.01(0.1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
