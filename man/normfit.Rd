\name{normfit}
\alias{normfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphical and statistical Gaussianity check}
\description{
Graphical and statistical check if data is Gaussian (4 Normality tests, QQ-plots, histograms, etc).
}
\usage{
normfit(data, plot.it = TRUE, volatility = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ a numeric vector of data values. }
   \item{plot.it}{ Should graphical inference be plotted (histogram, densities, qqplot, ...); default \code{TRUE}; otherwise only test results are returned. }
 \item{volatility}{ Should the squared data and its autocorrelation be plotted? Useful for financial time series to see if squares exhibit dependence (typically they do); default: \code{FALSE}}
}

\value{
A list containing 4 normality tests (each of class \code{htest})
  \item{ad}{Anderson Darling}
  \item{cvm}{Cramer-von-Mises}
  \item{sf}{Shapiro-Francia}
  \item{sw}{Shapiro-Wilk}
}
\references{
Thode Jr., H.C. (2002): \dQuote{Testing for  Normality}. Marcel Dekker, New York.
}
\author{Georg M. Goerg}

\seealso{\code{\link[nortest]{ad.test}}, \code{\link[stats]{shapiro.test}}, \code{\link[nortest]{sf.test}},\code{\link[nortest]{cvm.test}}}

\examples{
y=rLambertW(200, theta=c(0.1, 0, 1))
normfit(y)

fit=IGMM(y)
summary(fit)

fitl=MLE_LambertW(y)
plot(fitl)
x=get.input(y, fitl$theta)

normfit(y)
x11()
normfit(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
\keyword{hplot}
