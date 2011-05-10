\name{skewness_test}
\alias{skewness_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{One-sample Kolmogorov-Smirnov test for student-t distribution}
\description{
Performs a test for the null hypothesis of symmetry \eqn{H_0: \delta_l = \delta_r} versus the alternative of asymmetry. This can be done by a Wald test of the linear restriction \eqn{H_0: \delta_l - \delta_r = 0} or a likelihood ratio test.
}
\usage{
skewness_test(obj, method = "Wald")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{ an object of class \code{LambertW_fit} with \code{type = "hh"} or sequence of observation. If data is passed to the function, then an asymmetric Lambert W \eqn{\times} Gaussian distribution (\code{distname = "normal"}) with two tail parameters (\code{"hh"}) is fit to the data internally and then used as the new \code{obj}.}
  \item{method}{ what kind of test: the \code{"Wald"} or the likelihood \code{"ratio"} test}
}

\value{
  A list of class "htest" containing the following components:
  \item{statistic}{the value of the test statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string describing the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
}

\author{Georg M. Goerg}

\examples{
# skewed data
yy = rLambertW(n = 1000, delta = c(0.1, 0.2), beta=c(2,1), distname="normal")
fit.ml = MLE_LambertW(yy, type="hh", distname="normal")
summary(fit.ml)
skewness_test(fit.ml, "ratio")
skewness_test(fit.ml, "Wald")

# symmetric data 
yy = rLambertW(n = 1000, delta = c(0.2, 0.2), beta=c(2,1), distname="normal")
fit.ml = MLE_LambertW(yy, type="hh", distname="normal")
summary(fit.ml)
skewness_test(fit.ml, "ratio")
skewness_test(fit.ml, "Wald")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
