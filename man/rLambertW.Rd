\name{rLambertW}
\alias{rLambertW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Random generation for Lambert W RV}
\description{
Random generation for Lambert W RV
}
\usage{
rLambertW(n, theta = c(0, 0, 1), distname = "normal", innov = NULL, ZmUv = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ number of observations}
  \item{theta}{ parameter vector }
  \item{distname}{distribution}
  \item{innov}{pre-specified input. optional.}
  \item{ZmUv}{ Should the mean and stand. dev. of X be chosen such that Y is a zero mean, unit variance RV. default FALSE}
  \item{\dots}{ ... }
}

\value{
data y
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{optimize}
