\name{LambertW_fit-methods}
\alias{summary.LambertW_fit}
\alias{print.summary.LambertW_fit}
\alias{print.LambertW_fit}
\alias{plot.LambertW_fit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Methods for output of LambertW estimators}
\description{
Displays the output (class \code{LambertW_fit}) of the \code{\link{MLE_LambertW}} or \code{\link{IGMM}} estimator in a well-arranged and informative way: parameter estimates \eqn{\widehat{\theta}}, standard errors (including 'significance stars'), theoretical support (only for \code{type="s"}), skewness tests, etc.; nice formating (\code{print}), and \code{plot}s (see Details and Examples).
}
\usage{
\method{summary}{LambertW_fit}(object, ...)
\method{print}{summary.LambertW_fit}(x, ...)
\method{print}{LambertW_fit}(x, ...)
\method{plot}{LambertW_fit}(x, QQ=FALSE, a= NULL, b = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ object of class \code{LambertW_fit}}
  \item{x}{ object of class \code{LambertW_fit} }
  \item{a}{ left limit from where cdf and pdf are plotted}
  \item{b}{ right limit until cdf and pdf are plotted}
  \item{QQ}{ should a LambertW QQ plot be displayed? default \code{FALSE} }
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
\code{print.LambertW_fit} prints only very basic information about \eqn{\widehat{\theta}} (to prevent an overload of data/information in the console when executing an estimator).

\code{print.summary.LambertW_fit} tries to be smart about formatting the coefficients, standard errors, etc. and additionally gives "significance stars".

\code{plot.LambertW_fit} plots a (1) histogram, (2) empirical density of the data \code{y}. These are compared (3) to the theoretical \eqn{F_X(x \mid \widehat{\boldsymbol \beta})} and (4) Lambert W \eqn{\times} \eqn{F_X(\cdot \mid \widehat{\boldsymbol \beta})} densities.
}
\value{
\code{summary} gives an object of class \code{summary.LambertW_fit}. A list containing
  \item{call}{function call}
  \item{coefmat}{matrix with 4 columns: \eqn{\widehat{\theta}}, its standard errors, t-statistic, and corresponding (two-sided) p-values}
  \item{distname}{name of the input distribution}
  \item{n}{number of observations}
  \item{data}{original data set (\code{y})}
  \item{input}{back-transformed data}
  \item{support}{support of Y (only for \code{type = "s"}}
  \item{data.range}{empirical data range}
  \item{method}{estimation method}
  \item{hessian}{Hessian at the optimum. Numerically obtained for \code{method = "MLE"}; for \code{method = "IGMM"} only a diagonal-matrix approximation taken from the covariance matrix obtained by simulations for \eqn{n = 1000} observations.}
  \item{p_1}{Probability that one observation was caused by input lying on the non-principal branch; see \code{\link{p_1}}; only for \code{type = "s"}.} 
  \item{p_1n}{Probability that one or more of the n observation was caused by input lying on the non-principal branch; see \code{\link{p_1}}; only for \code{type = "s"}.} 
  \item{symmetry_pval}{the p-value for the test of identical left and right tail paramters by the Wald test (see \code{\link{skewness_test}}); only for \code{type = "hh"}.} 
}
\references{Goerg, G.M. (2011a). \dQuote{Lambert W Random Variables - A New Family of Generalized Skewed Distributions with Applications to Risk Estimation}. Forthcoming in the Annals of Applied Statistics (arxiv.org/abs/0912.4554).}
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
plot(fit.ml, a = 12, b = 40)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print}
\keyword{plot}
