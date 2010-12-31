\name{LambertW_parameters-utils}
\alias{check_LambertW_parameters}
\alias{complete_LambertW_parameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Checks if parameters describe a well-defined distribution}
\description{
\code{check_LambertW_parameters} checks if parameters \eqn{\alpha, \beta, \gamma, \delta} describe a well-defined Lambert W distribution. Prints out a warning if they do not, or if number of parameters given is too large/small for the specified distribution.

\code{complete_LambertW_parameters} completes missing values in a parameters list so users don't have to specify everything in detail. If not supplied, then the following will be set: \code{alpha = 1}, \code{gamma = 0}, and \code{delta = 0}. If \code{beta} is not specified, but an object of class \code{LambertW_input} is passed on, then \code{beta} will be set to the value of the input \code{beta}.
}
\usage{
check_LambertW_parameters(alpha = 1, beta = NULL, gamma = 0, delta=0, distname = NULL)
complete_LambertW_parameters(parameters = list(beta = c(0,1)), input = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alpha}{ exponent; default: \code{1} }
  \item{distname}{ name of the input distribution \eqn{F_X(x \mid \boldsymbol \beta)}; default: \code{NULL}.}
  \item{beta}{ parameter vector \eqn{\boldsymbol \beta} of the input distribution; default: \code{NULL} }
  \item{gamma}{ skewness parameter; default: \code{0} }
  \item{delta}{ heavy-tail parameter (= Tukey's h in case of a normal distribution); default: \code{0} }
  \item{parameters}{ a (probably incomplete) parameter list; default: \code{list(beta = c(0,1))} }
  \item{input}{an optional argument of \code{LambertW_input} }
}

\value{
\code{check_LambertW_parameters} prints out a warning if parameters do not define a proper distribution, or if number of parameter vector \eqn{\boldsymbol \beta} given is too large/small for the specified distribution; does nothing if parameters define a proper distribution.


\code{complete_LambertW_parameters} returns a list containing the arguments
  \item{alpha}{ exponent; default: \code{1} }
  \item{beta}{ parameter vector \eqn{\boldsymbol \beta} of the input distribution; default: \code{(0,1)} }
  \item{gamma}{ skewness parameter; default: \code{0} }
  \item{delta}{ heavy-tail parameter (= Tukey's h in case of a normal distribution); default: \code{0} }
}
\references{Goerg, G.M. (2011a). \dQuote{Lambert W Random Variables - A New Family of Generalized Skewed Distributions with Applications to Risk Estimation}. In review at the AOAS (arxiv.org/abs/0912.4554))}
\author{Georg M. Goerg}

\examples{
check_LambertW_parameters(beta= c(1,1,-1), distname = "t")
check_LambertW_parameters(beta= c(1,1,-1), distname = "normal")

check_LambertW_parameters(beta= c(1,1), distname = "normal") # ok

params = list(beta = c(2,1), delta = c(0.3)) # here alpha and gamma are missing
complete_LambertW_parameters(params) # plugged in with default values
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
