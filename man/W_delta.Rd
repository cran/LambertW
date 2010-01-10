\name{W_delta}
\alias{W_delta}
\alias{W_delta_1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Inverse transformation for Lambert W random variables}
\description{
Inverse transformation for Lambert W RVs. Principal and non-principal branch.
}
\usage{
W_delta(z, delta = 0)
W_delta_1(z, delta = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ a numeric vector of real values. }
  \item{delta}{ skewness parameter; by default \code{delta = 0}, which implies \code{W_delta(z) = W_delta_1(z) = z}. }
}

\details{
A Lambert W RV is defined using the transformation
\deqn{
z = u \exp(\delta u)=:H_{\delta}(u), \quad \delta \in R.
}
The function \code{W_delta(z)} (and \code{W_delta_1(z)}) are the inverse functions of this transformation. If \eqn{\delta = 0}, then \eqn{z = u} and the inverse transformation also equals the identity.

If \eqn{\delta \neq 0}, the inverse transformation can be computed by
\deqn{
W_{\delta}(z) = \frac{1}{\delta} W(\delta z).
}

Same holds for \code{W_delta_1(z)}.
}

\value{
Computes \eqn{\frac{1}{\delta} W(\delta z)}. If \eqn{z} is a vector, so is the output.
}

\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
