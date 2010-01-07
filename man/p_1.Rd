\name{p_1}
\alias{p_1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Non-principal branch probability }
\description{
Computes the probability that one (or more) \eqn{U} lie in the non-principal branch region. See Details.
}
\usage{
p_1(delta, distname = "normal", nu = NULL, n=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{delta}{ skewness parameter }
  \item{distname}{ character string stating the input distribution: \code{"t"} or \code{"normal"}. }
  \item{nu}{ degrees of freedom (if \code{distname="t"}) }
  \item{n}{ number of RVs/observations; default \code{n=1}}
}

\details{
For one observation this probability equals
\deqn{
p_{-1}(\delta, n=1) = P\left(U < -\frac{1}{|\delta|}\right),
}
where \eqn{U \sim F_U(u)} is a zero-mean, unit-variance RV.

For \eqn{N} independent RVs \eqn{U_1, \ldots, U_N}, it is interesting to know the probability that all of them lie in the principal region, or equivalently one minus the probability that at least one of the observations lies in the non-principal branch. Hence, for \eqn{N > 1} define,
\deqn{
p_{-1}(\delta, n=N) = P\left(U_i \geq -\frac{1}{|\delta|}, \forall i \right) = 1 - P\left(U_i < -\frac{1}{|\delta|}, \forall i \right) = 1- (1-p_{-1}(\delta, n=1))^N.
}
Hence, the cdf of a geometric RV can be used (\code{\link[stats]{pgeom}}).

For \eqn{N=1} this coincides with the original definition.
}

\value{
\eqn{p_{-1}} for \code{n} observations (in practice very small given empirical evidence).
Numerical problems can occur for \eqn{\delta < 0.03} (always gives \code{0}; rounding errors).
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}


\examples{
# for n=1 observation
p_1(0) # this probability is identical to 0
p_1(0.01) # in theory not identical to 0; but machine precision too low
p_1(0.05) # extremely small
p_1(0.1) # not 0, but very small; delta = 0.1 typical value for sample data
p_1(1.5) # 1 out of 4 samples is a non-principal input; delta=1.5 is not common in practice

# for n=100 observations
p_1(0, n=100) # this probability is identical to 0
p_1(0.1, n=100) # still small
p_1(1.5, n=100) # we can be almost 100% sure (rounding errors) that at least one of the y_i observations was caused by 			#an input in the non-principal branch
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
