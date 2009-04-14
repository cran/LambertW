\name{KS.test.t}
\alias{KS.test.t}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{One-sided Kolmogorov - Smirnov test for student-t distribution}
\description{
Performs a one-sided KS test if data is student-t with nu degrees of freedom, mean \eqn{mu_x}, and stand. dev. \eqn{sigma_x}. If these parameters are not specified, the MLE estimates for the data are used (see \code{fitdistr}).
}
\usage{
KS.test.t(y, theta = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ data }
  \item{theta}{ theta incl. df parameter nu}
}

\value{
  A list with class "htest" containing the following components:
  \item{statistic}{the value of the Kolomogorv-Smirnov statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "One-sample Kolmogorov-Smirnov test student-t" plus rounded parameter values.}
  \item{data.name}{a character string giving the name(s) of the data.}
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}
\seealso{\code{\link{fitdistr}}, \code{\link{ks.test}}}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
