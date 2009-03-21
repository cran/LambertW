\name{get.input}
\alias{get.input}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Back-transform Y to X}
\description{
 Given the data y and theta, this function calculates the back transformed data \eqn{x_{delta}}. 
}
\usage{
get.input(y, theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ data }
  \item{theta}{ parameter vector }
}

\value{
 \item{u}{Centered and normalized data u}
 \item{x}{Original input data x}
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}


\examples{
y=rLambertW(n=1000, c(0.1,0,1))
fit.gmm=IGMM(y)
summary(fit.gmm)

x=get.input(y, fit.gmm$theta)$x
plot(x)
normfit(x)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
