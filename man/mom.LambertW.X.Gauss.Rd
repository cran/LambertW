\name{mom.LambertW.X.Gauss}
\alias{mom.LambertW.X.Gauss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Moments of Y for Gaussian input}
\description{
Computes the moments of Y given theta and Gaussian input
}
\usage{
mom.LambertW.X.Gauss(theta, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{ parameter vector }
  \item{\dots}{ ...}
}

\value{
First four moments of Y
  \item{mu_y}{mean}
  \item{sigma_y}{stand. dev.}
\item{skew}{Pearson skewness}
\item{kurt}{Pearson kurtosis}
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}

\seealso{~~objects to See Also as \code{\link{help}}, ~~}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
