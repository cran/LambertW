\name{vec.norm}
\alias{vec.norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Lp norm of a vector }
\description{
Computes the \eqn{\mathbf{L}^p} norm of an n-dimensional (real/complex) vector.
}
\usage{
vec.norm(x, p=2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ n-dimensional vector (possibly complex values) }
  \item{p}{ which norm? default: \code{p=2} (Euclidean norm) }
}

\details{
The \eqn{\mathbf{L}^p} norm of a vector \eqn{\mathbf{x} \in \mathbf{C}^n} equals
\deqn{
\left|\left| \mathbf{x} \right|\right|_p = \left( \sum_{i=1}^n \left| x_i \right|^p \right)^{1/p},
}
where \eqn{\left| x_i \right|} is the absolute value of \eqn{x_i}. For \eqn{p=2} we get the Euclidean norm; for \eqn{p=1} the Manhattan distance.
}

\value{
the norm of \eqn{\mathbf{x}}
}
\author{Georg M. Goerg}
\examples{
a = c(3,-4)

# Pythagoras
vec.norm(a)
# did not know Manhattan.
vec.norm(a, p=1)

### unit circle?
x = exp(1i*runif(20, -pi,pi))
plot(x)
sapply(x, vec.norm) # yes, indeed!
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
