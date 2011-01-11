\name{LambertW-package}
\alias{LambertW-package}
\alias{LambertW}
\docType{package}
\title{Lambert W Random Variables}
\description{

The Lambert W framework is a new generalized way to analyze skewed, heavy-tailed data. Lambert W random variables (RV) are based on an input/ouput framework where the input is a RV X with distribution \eqn{F_X(x \mid \boldsymbol \beta)}, and the output Y = func(X) has similar properties as X (but slightly skewed or heavy-tailed). Then Y has a 'Lambert W \eqn{\times F_X(\cdot \mid \boldsymbol \beta)}' distribution - for details see the References.

Implemented distributions (\code{distname = ...}) are:  \code{"cauchy"}, \code{"chisq"}, \code{"exp"}, \code{"normal"}, \code{"t"}, \code{"unif"}. To be extended.

If your current empirical work requires another distribution, then a modular toolkit implementation (\code{\link{create_LambertW_input}} and \code{\link{create_LambertW_output}}) allows you to define your own Lambert W x 'my favorite distribution' and use it in the analysis of real world data.

This package contains many functions to perform an adequate analysis of skewed and heavy-tailed data: data can be simulated (\code{\link{rLambertW}}), parameters can be estimated (\code{\link{IGMM}} and \code{\link{MLE_LambertW}}), quantiles can be computed (\code{\link{qLambertW}}), and results presented/plotted in a 'nice' way (\code{\link{plot.LambertW_fit}} and \code{\link{print.LambertW_fit}}).

Probably the most important function is \code{\link{Gaussianize}}, which works the same way as the R function \code{\link[base]{scale}} but actually makes your data Gaussian. 

This packages is a based on the notation and definitions of the papers listed in the References (Goerg (2011a) and Goerg (2011b)); until their final publication these functions, definitions, and names may be subject to change. I will not include these 2 in the descriptions of each single function, unless they are explicitly referenced to in the function's description.

If you use this package in your work please cite it. If you want, you can send me your 'Lambert W \eqn{\times} my favorite distribution' (with a corresponding paper reference which will be included.) implementation and I will make it part of the standard LambertW package.

Comments, suggestions, code improvements/accelerations, implementation of new input distributions, bug reports, etc. are more than welcome: feel free to contact me.
}

\author{
Author and maintainer: Georg M. Goerg <gmg@stat.cmu.edu>
}
\references{
Goerg, G.M. (2011a). \dQuote{Lambert W Random Variables - A New Family of Generalized Skewed Distributions with Applications to Risk Estimation}. In review at the AOAS (http://arxiv.org/abs/0912.4554).

Goerg, G.M. (2011b). \dQuote{The Lambert Way to Gaussianize skewed, heavy-tailed data with the inverse of Tukey's h transformation as a special case.}. In preparation for submission (http://arxiv.org/abs/1010.2265).
}

\examples{
## Replicate parts of the statistical analysis in the References (2011a)
data(AA)
attach(AA)
X=AA[AA$sex=="f",]
y=X$bmi

op=par(no.readonly=TRUE)
normfit(y)

fit.gmm=IGMM(y, type="s")
summary(fit.gmm) # gamma is significant and positive
plot(fit.gmm)
# Comparison of estimated theoretical and sample moments
TAB = rbind(rbind(mLambertW(beta = fit.gmm$tau[1:2], gamma = fit.gmm$tau["gamma"], distname="normal")), cbind(mean(y), sd(y), skewness(y), kurtosis(y)))
rownames(TAB) = c("Theoretical (IGMM)", "Sample")
TAB

x=get.input(y, fit.gmm$tau)
normfit(x) # input is normal -> fit a Lambert W x Gaussian by maximum likelihood

fit.ml=MLE_LambertW(y, type="s", distname="normal")
summary(fit.ml)
plot(fit.ml)

## a finance example

M = ts(EuStockMarkets[201:1802,])
R = ts(diff(log(M))*100)

plot(R)

skewness(R[,"SMI"]) ## negative skewness
kurtosis(R[,"SMI"]) ## very high excess kurtosis

normfit(R[,"SMI"], vol = TRUE)

fit.SMI=IGMM(R[,"SMI"], type="s")
summary(fit.SMI) ## negative but small gamma

x = get.input(R[,"SMI"], fit.SMI$tau)
normfit(x, vol = TRUE) # symmetric, but still not normal -> try a skewed Lambert W x t distribution

## MLE for skewed Lambert W x t
fitMLt.SMI = MLE_LambertW(R[,"SMI"], distname = "t", type="s")
summary(fitMLt.SMI)
plot(fitMLt.SMI)

x = get.input(R[,"SMI"], fitMLt.SMI$tau)
plot(ts(cbind(R[,"SMI"], x)), main="")

## MLE for heavy-tail Lambert W x Gaussian ( = Tukey's h)
fitMLh.SMI = MLE_LambertW(R[,"SMI"], distname = "normal", type="h")
summary(fitMLh.SMI)
plot(fitMLh.SMI)

## MLE for skewed heavy-tail Lambert W x Gaussian ( = Tukey's hh)
fitMLhh.SMI = MLE_LambertW(R[,"SMI"], distname = "normal", type="hh")
summary(fitMLhh.SMI)
plot(fitMLhh.SMI)
x = get.input(R[,"SMI"], tau = fitMLhh.SMI$tau)
normfit(x) # Gaussianized the returns


}

\keyword{ package }
