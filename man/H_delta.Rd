\name{H_delta}
\alias{H_delta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{H transformation with delta}
\description{
Function underlying the Lambert W RV transformation: \eqn{H_{\delta}(u) = u \exp(\delta u)}.

This function is a wrapper for \eqn{1/\delta H(\delta u)}.
}
\usage{
H_delta(u, delta = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{u}{ a numeric vector of real values}
  \item{delta}{ skewness parameter; by default \code{delta = 0}, which implies \code{H_delta(u) = u}.}
}

\value{
Returns \eqn{u \exp(\delta u)}.
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}

\examples{
H_delta(1, delta=1)
W_delta(exp(1), delta=1)

H_delta(2) 					## default value is delta = 0: hence, H(u) = u
H_delta(0, delta=1) ## H_delta(0)= 0  for all delta
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
