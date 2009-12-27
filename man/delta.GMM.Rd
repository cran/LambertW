\name{delta.GMM}
\alias{delta.GMM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Estimate optimal delta}
\description{
Given \eqn{\mu_x} and \eqn{\sigma_x}, this function minimizes the distance (euclidean norm) between the theoretical skewness \eqn{\gamma(X)}, and the sample skewness of the back-transformed data \eqn{\widehat{\boldsymbol x}_{\theta}}. Note that only an interative application of this function will give a good estimate of \eqn{\theta} \eqn{\rightarrow} see \code{\link{IGMM}}.

A robust measure of the skewness is possible via the MedCouple estimator (see \code{\link{mc}}).
  }
\usage{
delta.GMM(y, robust = FALSE, c = mean(y), s = sqrt(var(y)), gamma_x = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a numeric vector of data values. }
  \item{robust}{ robust estimation of the sample skewness (see \code{mc})? default \code{FALSE} }
  \item{c}{ value that centers \code{y}; default: sample mean of \code{y} }
  \item{s}{ standardizing constant for \code{y-c}; default: sample standard deviation of \code{y} }
  \item{gamma_x}{ theoretical skewness. default: \code{0} }
}

\value{
A 3-dimensional vector containing the minimizing \eqn{\delta} given \code{c} and \code{s}; and the sample mean and sample standard deviation of the back-transformed data.
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}
\seealso{\code{\link{mc}} for a robust measure of skewness; \code{\link{IGMM}} for an iterative method to estimate all parameters}

\examples{
set.seed(1)
y = rLambertW(n=1000, theta=c(0.4,1,2)) ## very highly skewed
## note the big difference between the first and last iteration; 
## only a small change in delta, but a huge one in mu and sigma
delta.GMM(y) # after the first iteration
IGMM(y)$theta # after the final iteration; conversion has been reached.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{optimize}
