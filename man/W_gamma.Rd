\name{W_gamma}
\alias{W_gamma}
\alias{W_gamma_1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Inverse transformation for skewed Lambert W RVs}
\description{
Inverse transformation for skewed Lambert W RVs. Principal and non-principal branch.
}
\usage{
W_gamma(z, gamma = 0)
W_gamma_1(z, gamma = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ a numeric vector of real values. }
  \item{gamma}{ skewness parameter; by default \code{gamma = 0}, which implies \code{W_gamma(z) = W_gamma_1(z) = z}. }
}

\details{
A skewed Lambert W RV is defined by the transformation
\deqn{
z = u \exp(\gamma u)=:H_{\gamma}(u), \quad \gamma \in \mathbf{R}.
}
The function \code{W_gamma(z)} (and \code{W_gamma_1(z)}) are the inverse functions of this transformation. If \eqn{\gamma = 0}, then \eqn{z = u} and the inverse transformation also equals the identity.

If \eqn{\gamma \neq 0}, the inverse transformation can be computed by
\deqn{
W_{\gamma}(z) = \frac{1}{\gamma} W(\gamma z).
}

Same holds for \code{W_gamma_1(z)}.
}

\value{
Computes \eqn{\frac{1}{\gamma} W(\gamma z)}. If \eqn{z} is a vector, so is the output.
}

\author{Georg M. Goerg}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
