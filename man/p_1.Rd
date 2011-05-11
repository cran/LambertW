\name{p_1}
\alias{p_1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Non-principal branch probability }
\description{
Computes the probability that (at least) one (out of n) observation(s) of the latent variable \eqn{U} lie in the non-principal branch region. See Details.
}
\usage{
p_1(gamma = 0, beta = c(0,1), distname="normal", n = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gamma}{ skewness parameter }
  \item{beta}{ parameter vector of the input distribution }
  \item{distname}{ name of the input distribution; default: \code{"normal"}. }
  \item{n}{ number of RVs/observations; default \code{n=1}.}
}

\details{
The probability that one observation of the latent U lies in the non-principal region is at most (see References)
\deqn{
p_{-1}(\gamma, n=1) = P\left(U < -\frac{1}{|\gamma|}\right),
}
where \eqn{U} is the zero-mean, unit variance version of the input \eqn{X \sim F_X(x \mid \boldsymbol \beta)}.

For \eqn{N} independent RVs \eqn{U_1, \ldots, U_N}, we want to know the probability that at least one lies in the non-principal region. 
\deqn{
p_{-1}(\gamma, n=N) := P\left(U_i < -\frac{1}{|\gamma|} \; for \; at \; least \; one \; i \right)
}
By the rules of probability this equals (assuming independence)
\deqn{
P\left(U_i < -\frac{1}{|\gamma|}  \; for \; at \; least \; one \; i \right) = 1 - P\left(U_i \geq -\frac{1}{|\gamma|}, \forall i \right) = 1 - \prod_{i=1}^{N} P\left(U_i \geq -\frac{1}{|\gamma|} \right)
}
\deqn{
 = 1 - \prod_{i=1}^{N} \left(1 - p_{-1}(\gamma, n=1) \right)  = 1 - (1-p_{-1}(\gamma, n=1))^N.
}

Due to numerical stability the cdf of a geometric RV (\code{\link[stats]{pgeom}}) is used to evaluate the last expression.

Note that \eqn{1 - (1-p_{-1}(\gamma, n=1))^N} reduces to the original definition \eqn{p_{-1}(\gamma, n=1)} for \eqn{N=1}.
}

\value{
\eqn{p_{-1}} for \code{n} observations (in practice very small given empirical evidence).
Numerical problems can occur for \eqn{\gamma < 0.03} (always returns \code{0} due to rounding errors).
}
\references{Goerg, G.M. (2011a). \dQuote{Lambert W Random Variables - A New Family of Generalized Skewed Distributions with Applications to Risk Estimation}. Forthcoming in the Annals of Applied Statistics (arxiv.org/abs/0912.4554))}
\author{Georg M. Goerg}


\examples{
# for n=1 observation
p_1(0) # this probability is identical to 0
p_1(0.01) # in theory not identical to 0; but machine precision too low
p_1(0.05) # extremely small
p_1(0.1) # not 0, but very small; gamma = 0.1 typical value for sample data
p_1(1.5) # 1 out of 4 samples is a non-principal input; however, gamma=1.5 is not common in practice

# for n=100 observations
p_1(0, n=100) # this probability is identical to 0
p_1(0.1, n=100) # still small
p_1(0.3, n=100) # a bit more likely
p_1(1.5, n=100) # we can be almost 100% sure (rounding errors) that at least one of the y_i observations was caused by 			# an input in the non-principal branch

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
\keyword{univar}
