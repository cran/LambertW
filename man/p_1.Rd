\name{p_1}
\alias{p_1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Non-principal branch probability }
\description{
Computes the probability that the non-principal branch is important for the back-transformed data.
}
\usage{
p_1(delta, distname = "normal", nu = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{delta}{ skewness parameter }
  \item{distname}{ distribution }
  \item{nu}{ degrees of freedom (if \code{distname="t"}) }
  \item{\dots}{ ... }
}

\value{
probability
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}


\examples{
p_1(0.1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
