\name{pLambertW}
\alias{pLambertW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cumulative distribution function}
\description{
Cumulative distribution function (cdf) for Lambert W RV.
}
\usage{
pLambertW(y, theta = c(0, 0, 1), distname = "normal", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ value }
  \item{theta}{ parameter vector }
  \item{distname}{ input distribution }
  \item{\dots}{...}
}

\value{
value of the cdf at y.
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
