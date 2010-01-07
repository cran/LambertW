\name{LWest-methods}
\alias{summary.LWest}
\alias{print.summary.LWest}
\alias{print.LWest}
\alias{plot.LWest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Methods for output of LambertW estimators}
\description{
Useful information for estimate output (class \code{LWest}): parameter estimate \eqn{\widehat{\theta}}, standard errors, theoretical support, etc.; nice formating (\code{print}), and \code{plot}s (see Details).
}
\usage{
\method{summary}{LWest}(object, ...)
\method{print}{summary.LWest}(x, ...)
\method{print}{LWest}(x, ...)
\method{plot}{LWest}(x, QQ=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ object of class \code{LWest}}
  \item{x}{ object of class \code{LWest} }
  \item{QQ}{ should a LambertW QQ plot be displayed? default \code{FALSE} }
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
\code{print.LWest} prints only very basic information about \eqn{\widehat{\theta}} (to prevent an overload of data/information in the console when executing an estimator).

\code{print.summary.LWest} tries to be smart about formatting the coefficients, standard errors, etc. and additionally gives "significance stars".

\code{plot.LWest} plots a (1) histogram, (2) empirical density of the data \code{y}. These are compared to the theoretical Gaussian (or t) and Lambert W \eqn{\times} Gaussian (or t) densities (3) implied by \eqn{\widehat{\theta}}.
}
\value{
\code{summary} gives an object of class \code{summary.LWest}. A list containing
  \item{call}{function call}
  \item{coefmat}{matrix with 4 columns: \eqn{\widehat{\theta}}, its standard errors, t-statistic, and corresponding (two-sided) p-values}
  \item{distname}{input distribution; default: \code{"normal"}, alternative \code{"t"}}
  \item{n}{number of observations}
  \item{data}{original data set (\code{y})}
  \item{input}{back-transformed data}
  \item{support}{support of Y}
  \item{data.range}{empirical data range}
  \item{method}{estimation method}
  \item{hessian}{Hessian at the optimum. Numerically obtained for \code{method = "MLE"}; for \code{method = "IGMM"} only an approximation, as it is a diagonal matrix derived from the asymptotic covariance matrix obtained by simulations.}
  \item{p_1}{Probability that one observation was caused by input lying on the non-principal branch; see \code{\link{p_1}}.} 
  \item{p_1n}{Probability that one or more of the n observation was caused by input lying on the non-principal branch; see \code{\link{p_1}}.} 
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
