\name{LambertW-package}
\alias{LambertW-package}
\alias{LambertW}
\docType{package}
\title{Lambert W Random Variables}
\description{
Lambert W random variables (RV) offer a new way of dealing with slightly skewed data. It is based on an input/ouput framework - see Details and References. This package contains the most important functions and methods to perform an adequate analysis. Lambert W data can be simulated, parameters estimated, and results plotted in a "nice" way. Quantile functions allow a more realistic analysis and inference of skewed data.

Comments, suggestions, code improvements/accelerations, bug reports, ... are more than welcome: feel free to contact me.
}

\author{
Author and maintainer: Georg M. Goerg <gmg@stat.cmu.edu>
}
\references{
Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished
}

\examples{
## Replicate parts of the statistical analysis in the References
data(AA)
attach(AA)
X=AA[AA$sex=="f",]
y=X$bmi

op=par(no.readonly=TRUE)
normfit(y)

fit.gmm=IGMM(y)
summary(fit.gmm) # delta is significant and positive
plot(fit.gmm)
# Comparison of theoretical and empirical moments
mLambertW(fit.gmm$theta)
cbind(mean(y), sd(y), skewness(y), kurtosis(y))

x=get.input(y, fit.gmm$theta)
normfit(x) # input is normal -> MLE - Gaussian fit

fit.ml=MLE_LambertW(y)
summary(fit.ml)
plot(fit.ml)

## a finance example

M = ts(EuStockMarkets[201:1802,])
R = ts(diff(log(M))*100)

plot(M)
x11()
plot(R)

skewness(R[,"SMI"]) ## negative skewness
kurtosis(R[,"SMI"]) ## very high excess kurtosis

fit.SMI=IGMM(R[,"SMI"])
summary(fit.SMI) ## negative but small delta
x = get.input(R[,"SMI"], fit.SMI$theta)
normfit(x) # not normal
plot(ts(cbind(R[,"SMI"], x)), main="")

## MLE
fitML.SMI = MLE_LambertW(R[,"SMI"])
summary(fitML.SMI) 

fitMLt.SMI = MLE_LambertW(R[,"SMI"], "t")
summary(fitMLt.SMI)
plot(fitMLt.SMI, QQ=TRUE)
}

\keyword{ package }
