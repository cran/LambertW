\name{LambertW-package}
\alias{LambertW-package}
\alias{LambertW}
\docType{package}
\title{Lambert W Random Variables}
\description{
Lambert W random variables (RV) offer a new way of dealing with slightly skewed data. It is based on an input/ouput framework - for details see References. This package contains the most important functions to perform an adequate analysis. Lambert W data can be simulated, parameters estimated and results can be plotted in a proper way. Quantile functions allow a more realistic analysis and inference of skewed data.
}
\details{
\tabular{ll}{
Package: \tab LambertW\cr
Type: \tab Package\cr
Version: \tab 0.1.5\cr
Date: \tab 2009-03-21\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}

}
\author{
Georg M. Goerg <e0225792@student.tuwien.ac.at>

Maintainer: Georg M. Goerg <e0225792@student.tuwien.ac.at>
}
\references{
Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished
}

\examples{
data(AA)
attach(AA)
X=AA[AA$sex=="f",]
y=X$bmi

op=par(no.readonly=TRUE)
par(mfrow=c(2,1), mar=c(2,4,3,1))
plot(y)
hist(y, prob=TRUE, 15)
lines(density(y))
par(op)

fit.gmm=IGMM(y)
summary(fit.gmm) # Delta is significant and positive
plot(fit.gmm)
# Comparison of Theoretical and Empirical Moments
mom.LambertW.X.Gauss(fit.gmm$theta)
rbind(mean(y), sd(y), skewness(y), kurtosis(y))

x=get.input(y, fit.gmm$theta)$x
normfit(x)


plot(fit.gmm)
fit.ml=MLE_LambertW(y)
summary(fit.ml)
plot(fit.ml)
}

\keyword{ package }


