\name{Gaussianize}
\alias{Gaussianize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Gaussianizing matrix-like objects}
\description{
'Gaussianize' is probably the most useful function in this package. It works the same way as the \code{\link[base]{scale}} function in the R \code{base} package, but instead of just centering and scaling the data, it actually ``Gaussianizes'' the data. See Goerg (2011b) and Examples.
}
\usage{
Gaussianize(y, type = "h", method = "MLE")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a numeric matrix-like object }
  \item{type}{ what type of non-normality: symmetric heavy-tails \code{"h"}, skewed heavy-tails \code{"hh"}, or just skewed \code{"s"}.}
  \item{method}{what estimator should be used: \code{"MLE"} or \code{"IGMM"}. \code{"IGMM"} gives exactly Gaussian characteristics (kurtosis \eqn{\equiv} 3 for \code{"h"} or skewness \eqn{\equiv} 0 for \code{"s"}), \code{"MLE"} comes close to this.}
}

\value{
the Gaussianized matrix.

The numeric parameters of mean, scale, and skewness/heavy-tail parameters that were used in the Gaussianizing transformation are returned as attributes 'Gaussianized:mu', 'Gaussianized:sigma', 'Gaussianized:delta' (for \code{"h"}) or 'Gaussianized:delta_l' and 'Gaussianized:delta_r' (for \code{"hh"}), or 'Gaussianized:gamma' (for \code{"s"}).

Note that mean and scale in the Lambert W \eqn{\times} Gaussian framework are in general not identical to the center and scale of the \code{\link[base]{scale}} function.
}
\references{Goerg, G.M. (2011b). \dQuote{The Lambert Way to Gaussianize skewed, heavy-tailed data with the inverse of Tukey's h transformation as a special case.}. In preparation for submission (http://arxiv.org/abs/1010.2265).}
    
\author{Georg M. Goerg}

\examples{
set.seed(20)
y1 = rcauchy(n = 1000)
x1 = Gaussianize(y1)
normfit(x1) # Gaussianized a Cauchy!

start_from = 20
y_cum_avg = (cumsum(y1)/(1:length(y1)))[-c(1:start_from)]
x_cum_avg = (cumsum(x1)/(1:length(x1)))[-c(1:start_from)]

plot(c((start_from+1): length(y1)),y_cum_avg, type="l" , lwd = 2, main="Central limit theoreom in practice", xlab = "n", ylab="Cumulative sample average", ylim = c(min(y_cum_avg, x_cum_avg), max(y_cum_avg, x_cum_avg)))
lines(c((start_from+1): length(y1)), x_cum_avg, col=2, lwd=2)
abline(h = 0)

plot(x1, y1, xlab="Gaussian-like input", ylab = "Cauchy - output")


y2 = rLambertW(n = 1000, beta=c(3,1,6), delta = c(0,0.1), distname="t")
YY = cbind(y1, y2)
plot(YY)

XX = Gaussianize(YY, type="hh")
plot(XX)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
