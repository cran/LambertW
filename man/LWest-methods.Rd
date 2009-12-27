\name{LWest-methods}
\alias{summary.LWest}
\alias{print.summary.LWest}
\alias{plot.LWest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Methods for the output of a LambertW estimation}
\description{
Computes useful information for parameter estimates of the data (class \code{LWest}): Hessian, theoretical support, parameter estimates, etc.; nice formating (\code{print}), and \code{plot}s (see details).
}
\usage{
\method{summary}{LWest}(object, ...)
\method{print}{summary.LWest}(x, ...)
\method{plot}{LWest}(x, QQ=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ object of class \code{LWest}}
  \item{x}{ object of class \code{LWest} }
  \item{QQ}{ should a LambertW qq plot be displayed? default \code{FALSE} }
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
\code{print.summary.LWest} tries to be smart about formatting the coefficients, standard errors, etc. and additionally gives "significance stars".

\code{plot.LWest} plots a histogram and empirical density of the data \code{y}, and compares it to the estimated theoretical Gaussian (or t) based on \eqn{\widehat{\theta}}, and Lambert W \eqn{\times} Gaussian (or t) densities.
}
\value{
\code{summary} gives an object of class \code{summary.LWest}. A list containing
  \item{call}{function call}
  \item{coefmat}{matrix with 4 columns: \eqn{\widehat{\theta}}, its standard errors, t-statistic, and corresponding (two-sided) p-values}
  \item{distname}{input distribution; default: \code{"normal"}, alternative \code{"t"}}
  \item{n}{number of observations}
  \item{input}{back-transformed data}
  \item{support}{support of Y}
  \item{data.range}{empirical data range}
  \item{method}{estimation method}
  \item{hessian}{Hessian at the optimum. Numerically obtained for \code{method = "MLE"}; for \code{method = "IGMM"} the asymptotic values in the diagonal matrix \code{hessian}}
  \item{p_1}{Probability for the non-principal branch for one observation} 
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}
\examples{
data(AA)
attach(AA)

X=AA[AA$sex=="f",]
y=X$bmi

fit.ml=MLE_LambertW(y)
A=summary(fit.ml)
print(A)

plot(fit.ml)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print}
