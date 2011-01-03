\name{gamma_01}
\alias{gamma_01}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Input parameters to get a zero mean, unit variance output for a given gamma}
\description{
Given \eqn{\gamma}, this function computes the input mean \eqn{\mu_x(\gamma)} and standard deviation \eqn{\sigma_x(\gamma)} for Gaussian input \eqn{X \sim N(\mu_x(\gamma), \sigma_x^2(\gamma))}, such that the resulting skewed Lambert W Gaussian RV \eqn{Y} has zero mean and unit variance. 

The function works for any output mean and variance, but default values are \eqn{\mu_y = 0} and \eqn{\sigma_y = 1} since they are the most useful, e.g. to generate a standardized LambertW white noise sequence.

So far only for Gaussian input.
}
\usage{
gamma_01(gamma, mu_y=0, sigma_y=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gamma}{ skewness parameter }
  \item{mu_y}{ output mean; default: \code{0} }
  \item{sigma_y}{ output standard deviation; default: \code{1}}
}
\value{
The 3-dimensional vector (\eqn{\mu_x(\gamma)}, \eqn{\sigma_x(\gamma)},\eqn{\gamma}).
}

\author{Georg M. Goerg}

\examples{
gamma_01(0)	# for gamma = 0, input == output, therefore (0,1,0)
gamma_01(0.1)  	# gamma>0 (positively skewed): input mean must be slightly negative to get a zero-mean output
gamma_01(1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
