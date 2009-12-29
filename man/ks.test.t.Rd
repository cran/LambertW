\name{ks.test.t}
\alias{ks.test.t}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{One-sample Kolmogorov-Smirnov test for student-t distribution}
\description{
Performs a one-sided KS test for the null hypothesis that the data comes from a student-t distribution with \eqn{\nu} degrees of freedom, mean \eqn{\mu_x}, and standard deviation \eqn{\sigma_x}. If the parameters are not specified, the ML estimates given the data are used (see \code{fitdistr}).
}
\usage{
ks.test.t(y, theta = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a numeric vector of data values. }
  \item{theta}{ \eqn{\theta} including the degrees of freedom parameter \eqn{\nu}. by default \code{theta = NULL}, thus parameters will be estimated from the data \code{y}.}
}

\value{
  A list with class "htest" containing the following components:
  \item{statistic}{the value of the Kolomogorv-Smirnov statistic.}
  \item{p.value }{the p-value for the test.}
  \item{alternative }{a character string describing the alternative hypothesis.}
  \item{method}{the character string "One-sample Kolmogorov-Smirnov test student-t" plus rounded parameter values.}
  \item{data.name}{a character string giving the name(s) of the data.}
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}
\seealso{\code{\link{fitdistr}}, \code{\link{ks.test}}}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
