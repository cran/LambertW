\name{ks.test.t}
\alias{ks.test.t}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{One-sample Kolmogorov-Smirnov test for student-t distribution}
\description{
Performs a two-sided KS test for \eqn{H_0} that the data comes from a student-t distribution with \eqn{\nu} degrees of freedom, mean \eqn{\mu_x}, and standard deviation \eqn{\sigma_x} (not scale!). If the parameters are not specified, the ML estimates given the data are used (see \code{\link[MASS]{fitdistr}}).
}
\usage{
ks.test.t(y, theta = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a numeric vector of data values. }
  \item{theta}{ parameter vector including the degrees of freedom parameter: \eqn{\theta = (\delta, \mu_x, \sigma_x, \nu)}; default: \code{theta = NULL}, thus \eqn{\theta} is estimated from \code{y}.}
}

\value{
  A list with class "htest" containing the following components:
  \item{statistic}{the value of the Kolomogorv-Smirnov statistic.}
  \item{p.value }{the p-value for the test.}
  \item{alternative }{a character string describing the alternative hypothesis.}
  \item{method}{the character string "One-sample Kolmogorov-Smirnov test student-t" plus rounded parameter values.}
  \item{data.name}{a character string giving the name(s) of the data.}
}

\details{
This test does not give correct p-values if the parameters are estimated from the data. Future versions will include simulation based p-values. See \code{\link[stats]{ks.test}} and the references therein (Durbin (1973)).}

\author{Georg M. Goerg}
\seealso{\code{\link[MASS]{fitdistr}},\code{\link[stats]{ks.test}}}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
