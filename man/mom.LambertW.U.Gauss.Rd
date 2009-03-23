\name{mom.LambertW.U.Gauss}
\alias{mom.LambertW.U.Gauss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Moments of Y for Standard Gaussian input }
\description{
Given delta, compute the moments of Z.
}
\usage{
mom.LambertW.U.Gauss(delta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{delta}{skewness parameter}
}

\value{
First four moments of Z
  \item{mu_z}{mean}
  \item{sigma_z }{stand. dev.}
\item{skew}{Pearson skewness}
\item{kurt}{Pearson kurtosis}
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
