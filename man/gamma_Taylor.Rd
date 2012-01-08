\name{gamma_Taylor}
\alias{gamma_Taylor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Estimate of gamma by Taylor approximation}
\description{
Computes an initial estimate of \eqn{\gamma} based on the Taylor approximation of the skewness of Lambert W \eqn{\times} Gaussian RVs around \eqn{\gamma = 0}. See Details for the formula.

This is the initial estimate for \code{\link{IGMM}}.
  }
\usage{
gamma_Taylor(y = NULL, skewness_y = skewness(y), skewness_x = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a numeric vector of data values.}
  \item{skewness_y}{ skewness of \eqn{y}; default: empirical skewness of data \code{y}.}
  \item{skewness_x}{ skewness for input X; default: 0 (symmetric input).}
}

\value{
Estimate of \eqn{\gamma}.

Computed using the second order Taylor approximation around \eqn{\delta = 0}
\deqn{
\gamma_1(\gamma) = 6 \gamma + \mathcal{O}(\gamma^3).
}

Ignoring higher order terms and using the empirical estimate on the LHS we can solve for \eqn{\delta} (only using the positive root) to get
\deqn{
\widehat{\gamma}_{Taylor} = \frac{1}{6} \widehat{\gamma_1(\mathbf{y})},
}
where \eqn{\widehat{\gamma_1(\mathbf{y})}} is the empirical skewness of the data \eqn{\mathbf{y}}. 

As the Taylor approximation is only good in a neighborhood of \eqn{\gamma = 0}, the output of \code{gamma_Taylor} is restricted to the interval \eqn{-(0.25, 0.25)}.
}
\author{Georg M. Goerg}
\seealso{\code{\link{IGMM}} for an iterative method to estimate all parameters jointly.}

\examples{
set.seed(2)

yy = rLambertW(n=1000, beta=c(0,1), gamma = 0.1) ## a little skewness
gamma_Taylor(yy) # initial estimate is good because true delta = 0.2 close to 0, and empirical kurtosis well-defined.
gamma_GMM(yy) # best estimate
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{optimize}
