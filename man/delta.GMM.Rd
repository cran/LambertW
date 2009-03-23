\name{delta.GMM}
\alias{delta.GMM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{GMM estimation of delta only}
\description{
Given \eqn{mu_x} and \eqn{sigma_x}, it computes the value of delta such that the sample skewness of the back transformed data equals the theoretical one of X, gamma(X). In particular, for Gaussian and student-t input gamma(X) = 0 (default value), so this finds the delta that "symmetrizes" the backtransformed data.

A robust measure of the skewness is possible via the MedCouple estimator.
  }
\usage{
delta.GMM(y, robust = FALSE, c = mean(y), s = sqrt(var(y)), gamma_x = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ data }
  \item{robust}{ should the skewness be measured in a robust way? \code{robust=TRUE/FALSE}; default is \code{FALSE}}
  \item{c}{ the value that centers \code{y}}
  \item{s}{ standardizing constant for \code{y} }
  \item{gamma_x}{ theoretical skewness. default value \code{gamma_x = 0} }
  \item{...}{...}
}

\value{
the parameter vector theta
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{optimize}
