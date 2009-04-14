\name{IGMM}
\alias{IGMM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Iterative Generalized Method of Moments -- IGMM}
\description{
Computes via iterations this value of theta that generates input data x, as close as possible to the theoretical X. Implementation of the Algorithm described in Goerg (2009).
}
\usage{
IGMM(y, robust = FALSE, tol = 10^(-6), gamma_x = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ data }
  \item{robust}{ Should the skewness be measured in a robust way (see \code{mc})? default \code{FALSE} }
  \item{tol}{ tolerance level for stopping of iterations (conversion reached)}
  \item{gamma_x}{Theoretical skewness of input X; by default 0}
}

\value{
  \item{theta}{optimal theta}
  \item{iterations}{number of iterations}
  \item{call}{function call} 
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}


\examples{
x=rnorm(1000)
fit=IGMM(x)
summary(fit)

y=rLambertW(n=1000, c(0.1, 2,1))
fity=IGMM(y)
summary(fity)
plot(fity)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{iteration}
\keyword{optimize}
