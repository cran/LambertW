\name{LambertW-package}
\alias{LambertW-package}
\alias{LambertW}
\docType{package}
\title{
Lambert W simulation and estimation}
\description{
This packages offers the tools to handle Lambert W random variables.}
\details{
\tabular{ll}{
Package: \tab LambertW\cr
Type: \tab Package\cr
Version: \tab 0.1.0\cr
Date: \tab 2009-03-21\cr
License: \tab GPL 2\cr
LazyLoad: \tab yes\cr
}
~~ An overview of how to use the package, including the most important ~~
~~ functions ~~
}
\author{
Who wrote it

Maintainer: Georg M. Goerg <ggoerg@mat.puc.cl>
}
\references{
~~ Literature or other references for background information ~~
}

\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
data(AA)
attach(AA)
X=AA[AA$sex=="f",]
y=X$bmi
fit.gmm=IGMM(y)
x=get.input(y, fit.gmm$theta)

}
