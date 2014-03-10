\name{delta_Taylor}
\alias{delta_Taylor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Estimate of delta by Taylor approximation}
\description{
Computes an initial estimate of \eqn{\delta} based on the Taylor approximation of the kurtosis of Lambert W \eqn{\times} Gaussian RVs. See Details for the formula.

This is the initial estimate for \code{\link{IGMM}}.
  }
\usage{
delta_Taylor(y = NULL, kurtosis_y = kurtosis(y))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a numeric vector of data values.}
  \item{kurtosis_y}{ kurtosis of \eqn{y}; default: empirical kurtosis of data \code{y}.}
}

\value{
Estimate of \eqn{\delta}.

Computed using the second order Taylor approximation around \eqn{\delta = 0}
\deqn{
\gamma_2(\delta) = 3 + 12 \delta + 66 \delta^2 + \mathcal{O}(\delta^3).
}

Ignoring higher order terms and using the empirical estimate on the LHS we can solve for \eqn{\delta} (only using the positive root) to get
\deqn{
\widehat{\delta}_{Taylor} = \frac{1}{66} \cdot \left( \sqrt{66 \widehat{\gamma_2(\mathbf{y})} - 162}-6 \right),
}
where \eqn{\widehat{\gamma_2(\mathbf{y})}} is the empirical kurtosis of the data \eqn{\mathbf{y}}. 

Since the kurtosis is only defined for \eqn{\delta < 1/4}, the output of \code{delta_Taylor} is upper bounded by \eqn{1/4}.

}
\author{Georg M. Goerg}
\seealso{\code{\link{IGMM}} for an iterative method to estimate all parameters jointly.}

\examples{
set.seed(2)

# a little heavy-tailed (kurtosis does exist)
y = rLambertW(n=1000, beta=c(0,1), delta = 0.2)
# initial estimate is good because true delta = 0.2 close to 0 
# and empirical kurtosis well-defined
delta_Taylor(y)
delta_GMM(y) # best estimate

# very heavy-tails (like Cauchy)
y = rLambertW(n=1000, beta=c(0,1), delta = 1) 
# initial estimate very bad, because delta = 1 far from 0,
# and empirical kurtosis not well defined
delta_Taylor(y) 
delta_GMM(y) # best estimate
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{optimize}
