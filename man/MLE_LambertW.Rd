\name{MLE_LambertW}
\alias{MLE_LambertW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Maximum Likelihood}
\description{
MLE of Lambert W RV for Gaussian and student-t input.
}
\usage{
MLE_LambertW(y, distname = c("normal"), theta.0 = IGMM(y)$theta, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{y}{ data }
  \item{distname}{input distribution }
  \item{theta.0}{ initial value. By default the GMM estimate. }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
 }

\value{
 an object of class \code{LWest}
  \item{theta.0 }{ initial value}
  \item{theta}{ML estimate}
  \item{hessian}{Hessian at the optimum. Numerical}
 \item{call}{function call}
 \item{message}{message from the optimization method. what kind of convergence}
 \item{distname}{input distribution}
 \item{data}{data}
 \item{method}{estimation method. Here "Maximum Likelihood" }
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}


\examples{
x=rLambertW(200, theta=c(0.1, 0, 1))
fit=MLE_LambertW(x)
fit
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{optimize}
