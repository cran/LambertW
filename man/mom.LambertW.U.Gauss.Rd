\name{mom.LambertW.U.Gauss}
\alias{mom.LambertW.U.Gauss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Moments of Z for Standard Gaussian Input }
\description{
Given delta, computes the first 4 moments of Z for standard Gaussian input U.
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
