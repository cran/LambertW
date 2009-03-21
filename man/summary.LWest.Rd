\name{summary.LWest}
\alias{summary.LWest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summary of Lambert W estimation}
\description{
Given an object of class \code{LWest} calculates useful information, such as Hessian, theoretical support, ...
}
\usage{
\method{summary}{LWest}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ object of class \code{LWest}}
  \item{\dots}{...}
}

\value{
  \item{call}{function call}
  \item{coefmat}{matrix with 4 columns for the estimated theta, its standard errors, t-statistic and corresponding (two-sided)p-values}
  \item{distname}{distribution}
  \item{nobs}{number of observations}
  \item{input}{back-transformed data}
  \item{support}{support of Y}
  \item{data.range}{original data range}
  \item{method}{Estimation method used.}
  \item{hessian}{Hessian at the optimum. Only if \code{method} is MLE; otherwise \code{NULL}}
  \item{p_1}{Probability for the non-principal branch} 
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print}
