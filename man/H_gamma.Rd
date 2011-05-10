\name{H_gamma}
\alias{H_gamma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{H transformation with gamma}
\description{
Skewed Lambert W RV transformation: \eqn{H_{\gamma}(u) = u \exp(\gamma u)}.

This function is a wrapper for \eqn{H(\gamma u)/\gamma}.
}
\usage{
H_gamma(u, gamma = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{u}{ a numeric vector of real values}
  \item{gamma}{ skewness parameter; default \code{gamma = 0}, which implies \code{H_gamma(u) = u}.}
}

\value{
\eqn{u \exp(\gamma u)}.
}
\author{Georg M. Goerg}

\examples{
H_gamma(1, gamma=1)

H_gamma(2) 					## default value is gamma = 0: hence, H(u) = u
H_gamma(0, gamma=1) ## H_gamma(0)= 0  for all gamma
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
