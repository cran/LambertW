\name{G}
\alias{G}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Tukey's h transformation with h = 1}
\description{
 Tukey's h transformation with \eqn{h = 1} equals \eqn{G(u) = u \exp(\frac{1}{2} \cdot u^2) = z}.
}
\usage{
G(u)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{u}{ a numeric vector of real/complex values.}
}

\value{
Returns \eqn{z = u \exp(\frac{1}{2} \cdot u^2)} for \eqn{u \in C}. If \eqn{u} is a vector, so is \eqn{z}.
}
\references{Goerg, G.M. (2011b). \dQuote{The Lambert Way to Gaussianize skewed, heavy-tailed data with the inverse of Tukey's h transformation as a special case.}. In preparation for submission (http://arxiv.org/abs/1010.2265).}
\author{Georg M. Goerg}
\seealso{\code{\link{G_delta_alpha}}}
\examples{
G(0)
G(5)

plot(G, -2,2, type="l", xlab="u", ylab="G(u)", lwd=2)
abline(h=0)
abline(v=0)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
