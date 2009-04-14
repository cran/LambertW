\name{MLE_LambertW}
\alias{MLE_LambertW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Maximum Likelihood Estimation}
\description{
MLE of Lambert W RV for Gaussian and student-t input.
}
\usage{
MLE_LambertW(y, distname = c("normal"), theta.0 = IGMM(y)$theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ data }
  \item{distname}{input distribution. By defaul \code{"normal"}. Alternative \code{"t"}}
  \item{theta.0}{ Starting value for numerical optimization. By default the GMM estimate. }
}

\value{
An object of class \code{LWest}
 \item{data}{ the data}
  \item{theta.0 }{ initial value}
  \item{theta}{MLE for theta}
  \item{logLH}{log-likelihood function (argument for the \code{summary} function to calculate the Hessian numerically)}
 \item{call}{function call}
 \item{message}{message from the optimization method. What kind of convergence?}
 \item{distname}{input distribution}
 \item{method}{Estimation method. Here "MLE"}

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
