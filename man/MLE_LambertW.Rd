\name{MLE_LambertW}
\alias{MLE_LambertW}
\alias{MLE_LambertW.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Maximum Likelihood Estimation}
\description{
Maximum Likelihood Estimation (MLE) \eqn{\theta} for Lambert W RV for Gaussian and student-t input.
}
\usage{
MLE_LambertW(y, distname = c("normal"), theta.0 = IGMM(y)$theta, hessian=TRUE)
\method{MLE_LambertW}{default}(y, distname = c("normal"), theta.0 = IGMM(y)$theta, hessian=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a numeric vector of real values. }
  \item{distname}{ input distribution; default: \code{"normal"}, alternative \code{"t"}}
  \item{theta.0}{ starting value for numerical optimization; default: \code{\link{IGMM}} estimate.}
  \item{hessian}{ return the (numerically obtained) hessian matrix at the optimum?; default: \code{TRUE}}
}

\value{
An object of class \code{LWest}:
 \item{data}{ the data}
  \item{theta.0 }{ initial value}
  \item{theta}{MLE for \eqn{\theta}  }
  \item{logLH}{log-likelihood function (argument for the \code{summary} function to numerically calculate the Hessian)}
 \item{hessian}{Hessian matrix; used to calculate standard errors} 
\item{call}{function call}
 \item{message}{message from the optimization method. What kind of convergence?}
 \item{distname}{character string indicating the input distribution: \code{"t"} or \code{"normal"}.}
 \item{method}{Estimation method. Here \code{"MLE"}}

}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}

\examples{
data(AA)
attach(AA)

X=AA[AA$sex=="f",]
y=X$bmi

fit.ml=MLE_LambertW(y)
summary(fit.ml)
plot(fit.ml)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{optimize}
