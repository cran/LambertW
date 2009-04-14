\name{H_delta}
\alias{H_delta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Transformation with delta}
\description{
Function of the underlying transformation for Lambert W RV.
}
\usage{
H_delta(u, delta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{u}{ value in R }
  \item{delta}{ skewness parameter }
}

\value{
Returns z = u exp(delta * u).
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
