\name{normfit}
\alias{normfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphical and stastical Gaussianity check}
\description{
Performs a check whether data is Gaussian or not. Graphical and statistical inference.
}
\usage{
normfit(data, volatility = FALSE, plot.it = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ data }
  \item{volatility}{ Should the squared data and its autocorrelation be plotted. Useful for financial data to see if squares exhibit dependence (typically they do for asset returns)}
  \item{plot.it}{ Should graphical inference be plotted (histogram, densities, qqplot, ...); default \code{TRUE} }
}

\value{
Three results of normality tests are returned (each of class \code{htest})
  \item{ad}{Anderson Darling}
  \item{sw}{Shapiro-Wilk}
\item{sf}{Shapiro-Francia}
}
\references{
Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished

Thode Jr., H.C. (2002): \dQuote{Testing for  Normality}. Marcel Dekker, New York.
}



\author{Georg M. Goerg}


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
