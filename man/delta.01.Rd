\name{delta.01}
\alias{delta.01}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Input parameters to get a zero mean, unit variance output for a given delta}
\description{
This function computes the input mean \eqn{\mu_x(\delta)} and standard deviation \eqn{\sigma_x(\delta)} such that the resulting Lambert W Gaussian RV \eqn{Y} has zero mean, and unit variance for any given \eqn{\delta}. Although, in principle, the function allows any output mean and variance, the default values \eqn{\mu_y = 0} and \eqn{\sigma_y = 1} are the most useful, e.g. to generate a standardized LambertW white noise sequence.

So far only for Gaussian input.
}
\usage{
delta.01(delta, mu_y=0, sigma_y=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{delta}{ skewness parameter }
  \item{mu_y}{ output mean; default: \code{0} }
  \item{sigma_y}{ output standard deviation; default: \code{1}}
}
\value{
The 3-dimensional vector (\eqn{\delta},  \eqn{\mu_x(\delta)}, \eqn{\sigma_x(\delta)}).
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}

\examples{
delta.01(0)	# for delta = 0, input == output, therefore (0,0,1)
delta.01(0.1)  	# delta>0 (positively skewed): input mean must be 
		# slightly negative to get a zero-mean output
delta.01(1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
