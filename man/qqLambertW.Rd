\name{qqLambertW}
\alias{qqLambertW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ QQ - Plot for Lambert W}
\description{
A qq-plot with Lambert W - Gaussian (t) quantiles. 
}
\usage{
qqLambertW(y, theta = c(0, 0, 1), distname = "normal", plot.it = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ data }
  \item{theta}{parameter vector }
  \item{distname}{ input distribution }
  \item{plot.it}{ should it be plotted }
  \item{\dots}{ ... }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{x}{Theoretical quantiles (sorted)}
  \item{y}{Empirical quantiles (sorted)}
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
