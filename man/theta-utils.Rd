\name{theta-utils}
\alias{check_theta}
\alias{complete_theta}
\alias{starting_theta}
\alias{bounds_theta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Checks if parameters describe a well-defined distribution}
\description{
\code{check_theta} checks if parameters \eqn{\alpha, \beta, \gamma, \delta} describe a well-defined Lambert W distribution. Prints out a warning if they do not, or if number of parameters given is too large/small for the specified distribution.

\code{complete_theta} completes missing values in a parameters list so users don't have to specify everything in detail. If not supplied, then the following will be set: \code{alpha = 1}, \code{gamma = 0}, and \code{delta = 0}. If \code{beta} is not specified, but an object of class \code{LambertW_input} is passed on, then \code{beta} will be set to the value of the input \code{beta}.

\code{starting_theta} provides starting parameters for \eqn{\alpha}, \eqn{\boldsymbol \beta}, \eqn{\gamma}, and \eqn{\delta} to be used in ML (\code{\link{MLE_LambertW}}) estimation. These values are obtained by first finding the input \eqn{\widehat{\boldsymbol x}_{\widehat{\theta}}} generating the output \eqn{\boldsymbol y} by \code{\link{IGMM}}, and then getting the MLE of \eqn{\boldsymbol \beta} for this input data \eqn{\widehat{\boldsymbol x}_{\widehat{\theta}} \sim F_X(x \mid \boldsymbol \beta)}.

\code{bounds_theta} returns lower and upper bounds for \eqn{\theta} which are necessary to specify for \code{\link{MLE_LambertW}} estimation.
}
\usage{
check_theta(alpha = 1, beta = NULL, gamma = 0, delta=0, distname = NULL)
complete_theta(theta = list(beta = c(0,1)), input = NULL)
starting_theta(y, type = "h", distname="normal", fixed_theta = list(alpha = 1))
bounds_theta(distname = "normal", beta = NULL, type = "s", fixed_theta = list(alpha = 1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alpha}{ exponent; default: \code{1} }
  \item{beta}{ parameter vector \eqn{\boldsymbol \beta} of the input distribution; default: \code{NULL} }
  \item{gamma}{ skewness parameter; default: \code{0} }
  \item{delta}{ heavy-tail parameter (= Tukey's h in case of a normal distribution); default: \code{0} }
  \item{distname}{ name of the input distribution \eqn{F_X(x \mid \boldsymbol \beta)}.}
  \item{theta}{ a (probably incomplete) parameter list; default: \code{list(beta = c(0,1))} }
  \item{input}{an optional argument of \code{LambertW_input} }
  \item{y}{ a numeric vector of real values. }
  \item{type}{ type of distribution to be estimated: skewed Lambert W \code{"s"}; one-parameter heavy-tail Lambert W \code{"h"} or two-parameter heavy-tail Lambert W \code{"hh"} distribution. }
  \item{fixed_theta}{ a list of fixed parameters in the optimization; default only \code{alpha = 1}}
}

\value{
\code{check_theta} prints out a warning if \eqn{\theta} does not define a proper Lambert W \eqn{\times} F distribution, or if number of parameter vector \eqn{\boldsymbol \beta} given is too large/small for the specified distribution; does nothing if parameters define a proper distribution.

\code{complete_theta} returns a list containing the arguments
  \item{alpha}{ exponent; default: \code{1} }
  \item{beta}{ parameter vector \eqn{\boldsymbol \beta} of the input distribution; default: \code{(0,1)} }
  \item{gamma}{ skewness parameter; default: \code{0} }
  \item{delta}{ heavy-tail parameter (= Tukey's h in case of a normal distribution); default: \code{0} }

\code{starting_theta} returns a list containing the arguments
  \item{alpha}{ exponent; default: \code{1} }
  \item{beta}{ parameter vector \eqn{\boldsymbol \beta} of the input distribution; estimated from the recovered input data \eqn{\widehat{\mathbf{x}}_{\widehat{\tau}_{IGMM}}}  }
  \item{gamma}{ skewness parameter; if \code{type} is \code{"h"} or \code{"hh"} \code{gamma = 0}; estimated from \code{\link{IGMM}} }
  \item{delta}{ heavy-tail parameter (= Tukey's h in case of a normal distribution); if \code{type = "s"} \code{delta = 0} ; estimated from \code{\link{IGMM}}}

\code{bounds_theta} returns a list containing two vectors
  \item{lowerbounds}{ lower bounds for estimating \eqn{\theta} }
  \item{upperbounds}{ upper bounds for estimating \eqn{\theta} }
}
\references{Goerg, G.M. (2011a). \dQuote{Lambert W Random Variables - A New Family of Generalized Skewed Distributions with Applications to Risk Estimation}. In review at the AOAS (arxiv.org/abs/0912.4554))}
\author{Georg M. Goerg}

\examples{
check_theta(beta= c(1,1,-1), distname = "t")
check_theta(beta= c(1,1,-1), distname = "normal")

check_theta(beta= c(1,1), distname = "normal") # ok

params = list(beta = c(2,1), delta = c(0.3)) # here alpha and gamma are missing
complete_theta(params) # plugged in with default values

###
x=rnorm(1000)
starting_theta(x, distname="normal", type="h")
starting_theta(x, distname="normal", type="s")

# starting values for the skewed version of a Normal
y = rLambertW(n=1000, distname="exp", beta = 2, gamma = 0.1)
starting_theta(y, distname="exp", type="s")

# starting values for the heavy-tailed version of a Normal = Tukey's h
y=rLambertW(n=1000, beta = c(2,1), distname = "normal", delta = 0.2)
starting_theta(y, distname="normal", type = "h") #
summary(MLE_LambertW(y, distname="normal", type="h")) # quite close


###
bounds_theta(type="hh", distname="normal", beta = c(0,1), fixed_theta = list())

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
