\name{gamma_GMM}
\alias{gamma_GMM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Estimate optimal gamma}
\description{
This function minimizes the Euclidean distance between the theoretical skewness \eqn{\gamma_2(X)}, and the sample skewness of the back-transformed data \eqn{W_{\delta}(\boldsymbol z)} as a function of \eqn{\gamma} (see References). Note that only an interative application of this function will give a good estimate of \eqn{\gamma} \eqn{\rightarrow} see \code{\link{IGMM}}.

A robust measure of the asymmetry can also be used (see MedCouple estimator: \code{\link{mc}}).
  }
\usage{
gamma_GMM(z, skewness_x = 0,gamma.0 = (skewness(z) - skewness_x)/6, robust = FALSE, tol=.Machine$double.eps^0.5) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ a numeric vector of data values.}
  \item{gamma.0}{ starting value for \eqn{\gamma}; default: \code{(skewness(z) - skewness_x)/6} (the Taylor approximation - see References) }
  \item{skewness_x}{ theoretical skewness of the input \eqn{X}. default: \code{0} }
  \item{robust}{ indicator if robust estimation of the sample skewness (\code{\link{mc}}) should be used; default: \code{FALSE} }
 \item{tol}{ a positive scalar giving the tolerance at which the distance is considered close enough to zero to terminate the algorithm; default: \code{.Machine$double.eps^0.5} }
}

\value{
A list with two elements: 
\item{gamma}{ optimal \eqn{\gamma} for data \eqn{z} }
\item{iterations}{number of iterations}
}
\references{Goerg, G.M. (2011a). \dQuote{Lambert W Random Variables - A New Family of Generalized Skewed Distributions with Applications to Risk Estimation}. In review at the AOAS (arxiv.org/abs/0912.4554).}
\author{Georg M. Goerg}
\seealso{\code{\link{delta_GMM}} for the heavy-tail version of this function; \code{\link{mc}} for a robust measure of asymmetry; \code{\link{IGMM}} for an iterative method to estimate all parameters accurately}

\examples{
set.seed(1)
y = rLambertW(n=1000, beta = c(1,2), gamma = 0.1, distname="normal") ## very highly skewed

gamma_GMM(y) # after the first iteration
IGMM(y)$theta # after the final iteration; conversion has been reached.

IGMM(y)$TAU
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{optimize}
