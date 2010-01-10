\name{get.input}
\alias{get.input}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Back-transform Y to X}
\description{
 Given the data \code{y} and the parameter vector \eqn{\theta}, this function computes the back transformed (or "input") data \eqn{\widehat{\boldsymbol x}_{\theta}}.
}
\usage{
get.input(y, theta, return.u=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a numeric vector of data values. }
  \item{theta}{ parameter vector }
  \item{return.u}{ should the normalized input be returned; default: \code{FALSE}}
}

\value{
The estimated input data vector \eqn{\widehat{\boldsymbol x}_{\theta}} (solution of the principal branch of \code{\link{W_delta}}).

If \code{return.u = TRUE} then it returns a list with 2 vectors
 \item{u}{centered and normalized input u}
 \item{x}{input data \eqn{\widehat{\boldsymbol x}_{\theta}}}
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}


\examples{
set.seed(12)
y=rLambertW(n=1000, c(0.1,0,1))
normfit(y)

fit.gmm=IGMM(y)
summary(fit.gmm)

x=get.input(y, fit.gmm$theta)
plot(x)
normfit(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
