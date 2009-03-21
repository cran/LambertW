\name{qLambertW}
\alias{qLambertW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Quantile function}
\description{
Computes the quantiles given a probability level p.
}
\usage{
qLambertW(p, theta, distname = "normal")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{probability level alpha}
  \item{theta}{ parameter vector }
  \item{distname}{ input distribution }
}


\value{
value of z that results in this probability level alpha.
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
