\name{delta_GMM}
\alias{delta_GMM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Estimate optimal delta}
\description{
This function minimizes the Euclidean distance between the theoretical kurtosis \eqn{\gamma_2(X)}, and the sample kurtosis of the back-transformed data \eqn{W_{\delta}(\boldsymbol z)} as a function of \eqn{\delta} (see References). Note that only an interative application of this function will give a good estimate of \eqn{\delta} \eqn{\rightarrow} see \code{\link{IGMM}}.
  }
\usage{
delta_GMM(z, kurtosis_x = 3,delta.0 = max(0, (kurtosis(z) - kurtosis_x)/100),  tol=.Machine$double.eps^0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ a numeric vector of data values.}
  \item{kurtosis_x}{ theoretical kurtosis of the input X; default: \code{3} (\eqn{X \sim} Gaussian) }
  \item{delta.0}{ starting value for optimization; default: \code{max(0, (kurtosis(z) - kurtosis_x)/100)} }
 \item{tol}{ a positive scalar giving the tolerance at which the distance is considered close enough to zero to terminate the algorithm; default: \code{.Machine$double.eps^0.5} }
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
y = rLambertW(n=1000, beta=c(1,2), delta = 1) ## very heavy-tails (like a Cauchy)

delta_GMM(y) # after the first iteration
IGMM(y, type="h")$TAU # all iterations
## note the fast settling down to a neighborhood of the final solution
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{optimize}
