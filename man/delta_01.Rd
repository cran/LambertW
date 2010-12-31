\name{delta_01}
\alias{delta_01}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Input parameters to get a zero mean, unit variance output for a given delta}
\description{
Given \eqn{\delta}, this function computes the input mean \eqn{\mu_x(\delta)} and standard deviation \eqn{\sigma_x(\delta)} for Gaussian input \eqn{X \sim N(\mu_x(\delta), \sigma_x^2(\delta))}, such that the resulting heavy-tail Lambert W Gaussian RV \eqn{Y} has zero mean and unit variance. 

The function works for any output mean and variance, but default values are \eqn{\mu_y = 0} and \eqn{\sigma_y = 1} since they are the most useful, e.g. to generate a standardized LambertW white noise sequence.

So far only for Gaussian input and one-dimensional \eqn{\delta}.
}
\usage{
delta_01(delta, mu_y=0, sigma_y=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{delta}{ heavy-tail parameter ( = Tukey's h for Gaussian input) }
  \item{mu_y}{ output mean; default: \code{0} }
  \item{sigma_y}{ output standard deviation; default: \code{1}}
}
\value{
The 3-dimensional vector (\eqn{\mu_x(\delta)}, \eqn{\sigma_x(\delta)}, \eqn{\delta}).
}

\author{Georg M. Goerg}

\examples{
delta_01(0)	# for delta = 0, input == output, therefore (0,1,0)
delta_01(0.1)  	# delta > 0 (heavy-tails skewed): since Y is symmetric for all delta: mean = 0; however, sd must be smaller now 
delta_01(1/3) # only moments up to order 2 exist
delta_01(1) # variance does not exist anymore for this delta, thus NaN
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
