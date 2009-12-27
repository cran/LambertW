\name{normfit}
\alias{normfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphical and statistical Gaussianity check}
\description{
Graphical and statistical inference if data is Gaussian (3 Normality tests, QQ-plots, histograms, etc).
}
\usage{
normfit(data, volatility = FALSE, plot.it = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ a numeric vector of data values. }
  \item{volatility}{ Should the squared data and its autocorrelation be plotted? Useful for financial time series to see if squares exhibit dependence (typically they do)}
  \item{plot.it}{ Should graphical inference be plotted (histogram, densities, qqplot, ...); default \code{TRUE} }
}

\value{
A list containing three normality tests (each of class \code{htest})
  \item{ad}{Anderson Darling}
  \item{sw}{Shapiro-Wilk}
  \item{sf}{Shapiro-Francia}
}
\references{
Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished
Thode Jr., H.C. (2002): \dQuote{Testing for  Normality}. Marcel Dekker, New York.
}
\author{Georg M. Goerg}

\seealso{\code{\link[nortest]{ad.test}}, \code{\link[stats]{shapiro.test}}, \code{\link[nortest]{sf.test}}}

\examples{
y=rLambertW(200, theta=c(0.1, 0, 1))
normfit(y)

fit=IGMM(y)
summary(fit)

fitl=MLE_LambertW(y)
plot(fitl)
x.d=get.input(y, fitl$theta)$x

normfit(y)
x11()
normfit(x.d)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
\keyword{hplot}
