\name{H}
\alias{H}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Orginal function defining the Lambert W function}
\description{
 The Lambert W function \eqn{W(z)} is the inverse of this function: \eqn{H(u) = u \exp(u) = z}.
}
\usage{
H(u)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{u}{ a numeric vector of real/complex values.}
}

\value{
Returns \eqn{z = u \exp(u)} for \eqn{u \in C}. If \eqn{u} is a vector, so is \eqn{z}.
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}

\examples{
H(0)
W(0)

H(10)
W(H(10))

## at the critical point z = -1, both branches give the same inverse.
H(-1)
W(H(-1))
W_1(H(-1))

## going further to the left, the principal branch gives the 'wrong' solution; 
## the non-principal must be used
H(-10)
W(H(-10))
W_1(H(-10))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
