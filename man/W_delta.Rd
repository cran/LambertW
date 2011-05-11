\name{W_delta_alpha}
\alias{W_delta}
\alias{W_delta_alpha}
\alias{W_2delta}
\alias{W_2delta_alpha}
\alias{d1W_delta}
\alias{d1W_delta_alpha}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Inverse transformation for heavy-tail Lambert W RVs}
\description{
Inverse transformation for heavy-tail Lambert W RVs; inverse of Tukey's h transformation as a special case.
}
\usage{
W_delta(z, delta = 0)
W_delta_alpha(z, delta = 0, alpha = 1)
W_2delta(z, delta = c(0,1/5))
W_2delta_alpha(z, delta = c(0,1/5), alpha = 1)
d1W_delta(z, delta = 1)
d1W_delta_alpha(z, delta = 1, alpha = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ a numeric vector of real values. }
  \item{delta}{ heavy-tail parameter; by default \code{delta = 0.1}, which implies \code{W_delta(z) = z}. If a vector of length 2 is supplied, then \code{delta[1]} on the left, and \code{delta[2]} on the right (of the center) will be used.}
  \item{alpha}{ exponent in \eqn{(u^2)^{\alpha}}; default \code{alpha = 1}.}
}

\value{
Computes \eqn{sgn(z) \left(\frac{1}{\delta} W(\delta (z^2)^{\alpha}) \right)^{1/2 \alpha}}. If \eqn{z} is a vector, so is the output.
}
\references{
Goerg, G.M. (2011b). \dQuote{The Lambert Way to Gaussianize skewed, heavy-tailed data with the inverse of Tukey's h transformation as a special case.}. In preparation for submission (http://arxiv.org/abs/1010.2265).
}

\author{Georg M. Goerg}

\examples{
G(0)
W(0)

G(10)
W_delta_alpha(G(10), delta = 1, alpha=1) # the inverse
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
