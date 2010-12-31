\name{starting_parameters}
\alias{starting_parameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Provide starting parameters for MLE}
\description{
This function provides starting parameters for \eqn{\alpha}, \eqn{\boldsymbol \beta}, \eqn{\gamma}, and \eqn{\delta} to be used in ML (\code{\link{MLE_LambertW}}) estimation. These values are obtained by first finding the input \eqn{\widehat{\boldsymbol x}_{\widehat{\theta}}} generating the output \eqn{\boldsymbol y} by \code{\link{IGMM}}, and then getting the MLE of \eqn{\boldsymbol \beta} for this input data \eqn{\widehat{\boldsymbol x}_{\widehat{\theta}} \sim F_X(x \mid \boldsymbol \beta)}. 
}
\usage{
starting_parameters(y, type = "h", distname="normal", fixed_parameters = list(alpha = 1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a numeric vector of real values. }
  \item{type}{ type of distribution to be estimated: skewed Lambert W \code{"s"}; one-parameter heavy-tail Lambert W \code{"h"} or two-parameter heavy-tail Lambert W \code{"hh"} distribution. }
  \item{distname}{ input distribution; default: \code{"normal"}.}
  \item{fixed_parameters}{ a list of fixed parameters in the optimization; default only \code{alpha = 1}}
}

\value{
a list with the arguments
  \item{alpha}{ exponent; default: \code{1} }
  \item{beta}{ parameter vector \eqn{\boldsymbol \beta} of the input distribution }
  \item{gamma}{ skewness parameter; if \code{type} is \code{"h"} or \code{"hh"} \code{gamma = 0} }
  \item{delta}{ heavy-tail parameter (= Tukey's h in case of a normal distribution); if \code{type = "s"} \code{delta = 0} }
}
\author{Georg M. Goerg}
\examples{
x=rnorm(1000)
starting_parameters(x, distname="normal", type="h")
starting_parameters(x, distname="normal", type="s")

# starting values for the skewed version of a Normal
y = rLambertW(n=1000, distname="exp", beta = 2, gamma = 0.1)
starting_parameters(y, distname="exp", type="s")

# starting values for the heavy-tailed version of a Normal = Tukey's h
y=rLambertW(n=1000, beta = c(2,1), distname = "normal", delta = 0.2)
starting_parameters(y, distname="normal", type = "h") #
summary(MLE_LambertW(y, distname="normal", type="h")) # quite close
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{iteration}
\keyword{optimize}
