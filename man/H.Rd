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
\references{Corless, R. M., G. H. Gonnet, D. E. G. Hare, and D. J. Jeffrey (1993). \dQuote{On the Lambert W function}. preprint.}
\author{Georg M. Goerg}
\seealso{\code{\link{H_gamma}}}
\examples{
H(0)
H(10)

plot(H, -5, 0.5, type="l", xlab="u", ylab="z")
abline(h=0)
abline(v=0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
