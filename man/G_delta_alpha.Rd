\name{G_delta_alpha}
\alias{G_delta_alpha}
\alias{G_2delta_alpha}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Tukey's h transformation with delta}
\description{
Heavy-tail Lambert W RV transformation: \eqn{H_{\delta}(u) = u \exp(\frac{\delta}{2} (u^2)^{\alpha})}.
}
\usage{
G_delta_alpha(u, delta = 0, alpha = 1)
G_2delta_alpha(u, delta=c(0,1/5), alpha = c(1,1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{u}{ a numeric vector of real values}
  \item{delta}{ heavy tail parameter; default \code{delta = 0}, which implies \code{G_delta_alpha(u) = u}.}
  \item{alpha}{ exponent in \eqn{(u^2)^{\alpha}}; default \code{alpha = 1}.}
}

\value{
\eqn{u \exp(\frac{\delta}{2} (u^2)^{\alpha})}.
}
\author{Georg M. Goerg}

\examples{
G_delta_alpha(1, delta=1)

G_delta_alpha(2) ## default value is delta = 0: hence, G_delta(u) = u
G_delta_alpha(0, delta=1) ## G_delta(0) = 0 for all delta
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
