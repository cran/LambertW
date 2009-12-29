\name{p_1}
\alias{p_1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Non-principal branch probability }
\description{
Computes the probability that the non-principal branch caused the observed data. 
}
\usage{
p_1(delta, distname = "normal", nu = NULL, n=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{delta}{ skewness parameter }
  \item{distname}{ character string stating the input distribution: \code{"t"} or \code{"normal"}. }
  \item{nu}{ degrees of freedom (if \code{distname="t"}) }
  \item{n}{ number of observations; default \code{n=1}}
}

\details{
For one observation this probability equals
\deqn{
p_1(\delta, n=1) = P\left(U < -\frac{1}{|\delta|}\right),
}
where \eqn{U \sim F_U(u)} is a zero-mean, unit-variance random variable.

For \eqn{N} independent samples
\deqn{
p_1(\delta, n=N) = 1- (1-p_1(\delta, n=1))^N.
}

However, there are numerical problems (solution/fix in the next version).
}

\value{
The probability (in practice very small for empirical range of \eqn{\widehat{\delta}}).
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}


\examples{
p_1(0) # this probability is identical to 0
p_1(0.1) # not 0, but very small; delta = 0.1 typical value in sample data
p_1(1.5) # one out of 4 samples is a non-principal input; delta=1.5 is not common in practice
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
