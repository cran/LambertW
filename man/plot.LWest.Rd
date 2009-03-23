\name{plot.LWest}
\alias{plot.LWest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot Estimation results }
\description{
Plots a histogram, empirical density of the data, and compares it to the estimated Gaussian (t) and Lambert W - Gaussian (t) density.
}
\usage{
\method{plot}{LWest}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{object of type \code{LWest} }
  \item{\dots}{...}
}

\value{
Plot
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}


\examples{
x=rLambertW(200, theta=c(0.1, 0, 1))
fit=IGMM(x)
plot(fit)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
