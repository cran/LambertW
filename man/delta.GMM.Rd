\name{delta.GMM}
\alias{delta.GMM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{GMM estimation of delta only}
\description{
Given \eqn{\mu_x} and \eqn{\sigma_x}, this function computes such a \eqn{\delta} that the sample skewness of the back transformed data equals the theoretical one gamma(X). In particular, for Gaussian and student-t input gamma(X) = 0 (default value), so \code{delta.GMM} finds this delta that "symmetrizes" some given data \code{y}.

A robust measure of the skewness is possible via the MedCouple estimator.
  }
\usage{
delta.GMM(y, robust = FALSE, c = mean(y), s = sqrt(var(y)), gamma_x = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ data }
  \item{robust}{ Should the skewness be measured in a robust way? \code{robust=TRUE/FALSE}; default is \code{FALSE}}
  \item{c}{ the value that centers \code{y}; default value is the sample mean of \code{y} }
  \item{s}{ standardizing constant for \code{y-c}; default value is the sample standard deviation of \code{y} }
  \item{gamma_x}{ theoretical skewness. default value \code{gamma_x = 0} }
}

\value{
Parameter vector theta, where \eqn{\mu_x} and \eqn{\sigma_x} equal simply the sample moments of the back transformed data.
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}
\seealso{\code{\link{mc}} for a robust measure of skewness}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{optimize}
