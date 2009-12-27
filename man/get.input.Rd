\name{get.input}
\alias{get.input}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Back-transform Y to X}
\description{
 Given the data \code{y} and the parameter vector \eqn{\theta}, this function computes the back transformed data \eqn{\widehat{\boldsymbol x}_{\theta}}.
}
\usage{
get.input(y, theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a numeric vector of data values. }
  \item{theta}{ parameter vector }
}

\value{
 \item{u}{centered and normalized input u}
 \item{x}{input data x}
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}


\examples{
set.seed(12)
y=rLambertW(n=1000, c(0.1,0,1))
normfit(y)

fit.gmm=IGMM(y)
summary(fit.gmm)

x=get.input(y, fit.gmm$theta)$x
plot(x)
normfit(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
