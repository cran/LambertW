\name{normfit}
\alias{normfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphical and statistical Gaussianity check}
\description{
Graphical and statistical check if data is Gaussian (3 common Normality tests, QQ-plots, histograms, etc).

These tests are from the \code{nortest} package. See 'See Also'.
}
\usage{
normfit(data, volatility = FALSE,plot.it = TRUE,  pch = 1, legend = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ a numeric vector of data values. }
  \item{plot.it}{ Should graphical inference be plotted (histogram, densities, qqplot, ...); default \code{TRUE}; otherwise only test results are returned. }
 \item{volatility}{ Should the squared data and its autocorrelation be plotted? Useful for (financial) time series to see if squares exhibit dependence (for financial data they typically do); default: \code{FALSE}}
  \item{pch}{ argument for the plot of the original data; default \code{pch = 1}}
\item{legend}{ should legends be placed in the histogram/density plot; default: \code{TRUE}}
}

\value{
A list containing 3 normality tests (each of class \code{htest})
  \item{ad}{Anderson Darling}
  \item{sf}{Shapiro-Francia}
  \item{sw}{Shapiro-Wilk}
}
\references{
Thode Jr., H.C. (2002): \dQuote{Testing for  Normality}. Marcel Dekker, New York.
}
\author{Georg M. Goerg}

\seealso{\code{\link[nortest]{ad.test}}, \code{\link[stats]{shapiro.test}}, \code{\link[nortest]{sf.test}} in the nortest package}

\examples{
y=rLambertW(n = 1000, beta=c(3,4), gamma = 0.1, distname="normal")
normfit(y)

x = rnorm(n=1000)
normfit(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
\keyword{hplot}
