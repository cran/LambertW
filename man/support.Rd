\name{support}
\alias{support}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute the truncated support for skewed Lambert W distributions}
\description{
If the input \eqn{X \sim F} has support on the entire real line \eqn{(-\infty, \infty)}, then the skewed Lambert W \eqn{\times} F distribution has truncated support \eqn{[a,b]}, \eqn{a,b \in R \cup \pm \infty} depending on \eqn{\boldsymbol \beta} and (the sign of) \eqn{\gamma}.
}
\usage{
support(theta, gamma = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{ parameter vector with mean and variance (and \eqn{\gamma} as the third entry)}
  \item{gamma}{ an optional argument to pass \eqn{\gamma} directly to the function, instead of the indirect \code{theta}.}
}

\value{
Half-open interval on the real line (if \eqn{\gamma \neq 0}) for input with support on the entire real line. For \eqn{\gamma = 0} the support of Y is the same as for X. Heavy-tail Lambert W RVs are not affected by truncated support (since \eqn{\delta \geq 0} by definition).
}
\author{Georg M. Goerg}

\examples{
support(c(0,1,0)) # as gamma = 0
support(c(0,1,0.1)) # truncated on the left since gamma > 0
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
