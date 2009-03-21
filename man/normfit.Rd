\name{normfit}
\alias{normfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphical and stastical Gaussianity check}
\description{
Performs a check whether data is Gaussian or not. Graphical and statistical inference.
}
\usage{
normfit(data, volatility = FALSE, plot = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ data }
  \item{volatility}{ should the squared data and its autocorrelation be plotted}
  \item{plot}{ should graphical inference be plotted (histogram, densities, qqplot, ...) }
  \item{\dots}{ ...}
}

\value{
Three normality tests are returned (each of class htest)
  \item{ad}{Anderson Darling}
  \item{sw}{Shapiro-Wilk}
\item{sf}{Shapiro-Francia}
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}


\examples{
x=rLambertW(200, theta=c(0.1, 0, 1))
fit=IGMM(x)
plot(fit)
summary(fit)

fitl=MLE_LambertW(x)
normfit(x)
x.d=get.input(x, fitl$theta)$x
x11()
normfit(x.d)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
\keyword{hplot}
