\name{delta_GMM}
\alias{delta_GMM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Estimate optimal delta}
\description{
This function minimizes the Euclidean distance between the theoretical kurtosis \eqn{\gamma_2(X)}, and the sample kurtosis of the back-transformed data \eqn{W_{\delta}(\boldsymbol z)} as a function of \eqn{\delta} (see References). Note that only an interative application of this function will give a good estimate of \eqn{\delta} \eqn{\rightarrow} see \code{\link{IGMM}}.
  }
\usage{
delta_GMM(z, kurtosis_x = 3, skewness_x = 0, type ="h", delta.0 = delta_Taylor(z), 
tol=.Machine$double.eps^0.25, restricted = TRUE, bounds = c(-5,10))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ a numeric vector of data values.}
  \item{kurtosis_x}{ theoretical kurtosis of the input X; default: \code{3} (\eqn{X \sim} Gaussian) }
  \item{skewness_x}{ theoretical skewness of the input X. This will be only used if \code{type = "hh"}; default: \code{0} (\eqn{X \sim} symmetric) }
\item{type}{ what type of distribution should be estimated? heavy-tail Lambert W \code{"h"}; heavy-tails Lambert W \code{"hh"}. }
  \item{delta.0}{ starting value for optimization; default: \code{\link{delta_Taylor}}.}
 \item{tol}{ a positive scalar giving the tolerance at which the distance is considered close enough to zero to terminate the algorithm; default: \code{.Machine$double.eps^0.25} }
  \item{restricted}{indicator if the estimate for \eqn{\delta} should be restricted to the positive reals; default: \code{TRUE}}
    \item{bounds}{ bounds for the estimate of \eqn{\delta}; in practice \eqn{\delta} will rarely exceed 2; here it is set to a maximum of 10. On the lower end it is set to -1. If \code{restricted = TRUE} then the first element will be overwritten and set to 0.}
}

\value{
A list with two elements: 
\item{delta}{ optimal \eqn{\delta} for data \eqn{z} }
\item{iterations}{number of iterations}
}
\author{Georg M. Goerg}
\seealso{\code{\link{gamma_GMM}} for the skewed version of this function; \code{\link{IGMM}} for an iterative method to estimate all parameters jointly.}

\examples{
set.seed(2)
y = rLambertW(n=1000, beta=c(1,2), delta = 1) # very heavy-tails (like Cauchy)

delta_GMM(y) # after the first iteration
IGMM(y, type="h")$TAU # all iterations
## note the fast settling down to a neighborhood of the final solution
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{optimize}
