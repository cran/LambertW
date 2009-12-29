\name{vec.norm}
\alias{vec.norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Lp norm of a vector }
\description{
Computes the \eqn{L^p} norm of an n-dimensional vector.
}
\usage{
vec.norm(x, p=2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ n-dimensional vector }
  \item{p}{ which norm? default: \code{p=2} (Euclidean norm) }
}

\details{
The \eqn{L^p} norm of a vector \code{x} in \eqn{R^n} equals
\deqn{
\left|\left| x \right|\right|_p = \left( \sum_{i=1}^n \left| x_i \right|^p \right)^{1/p},
}
where \eqn{\left| x_i \right|} is the absolute value of \eqn{x_i}. For \eqn{p=2} we get the Euclidean norm; for \eqn{p=1} the so called Manhattan distance.
}

\value{
the norm of \eqn{x}.
}
\author{Georg M. Goerg}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
