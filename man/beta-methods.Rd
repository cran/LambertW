\name{beta-methods}
\alias{beta2tau}
\alias{beta_names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Functions for parameter vector beta of the input distribution}
\description{
The parameter vector \eqn{\boldsymbol \beta} describes the input distribution \eqn{F_X(x \mid \boldsymbol \beta)}. Depending on the distribution \eqn{\boldsymbol \beta} has different length and names: for example, for a \code{"normal"} distribution \code{beta} is a vector of length \eqn{2} describing \code{"mu"} and \code{"sigma"}; for an exponential distribution \code{beta} is a vector of length \eqn{1} (i.e. a number) describing the rate \code{"lambda"}.

However, any \eqn{\boldsymbol \beta} implies different mean and variance of \eqn{X}. Thus here are two functions that - depending on the input distribution - assign  \eqn{\boldsymbol \beta} its typical names given the distribution, and converts the parameter values to the implied \eqn{\tau}.
}
\usage{
beta2tau(beta, distname = c("normal"), gamma = 0, delta = 0, alpha = 1)
beta_names(distname = c("normal"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{  
	\item{distname}{ name of the distribution \eqn{F_X(x \mid \boldsymbol \beta)} (follows mostly the suffix of the R functions; exception \code{"normal"} instead of \code{"norm"}); default: \code{"normal"} }
  \item{beta}{ parameter vector  \eqn{\boldsymbol \beta} of the input distribution; specifications as they are for the R implementation of this distribution. For example, if  \code{distname = "exp"}, then \code{beta = 2} means that the rate of the exponential distribution equals \eqn{2}; if \code{distname = "normal"} then \code{beta = c(1,2)} means that the mean and standard deviation of the input Gaussian distribution are 1 and 2, respectively. }

  \item{delta}{ heavy-tail parameter (= Tukey's h in case of a normal distribution); default: \code{0}. }
  \item{gamma}{ skewness parameter; default: \code{0}. }
  \item{alpha}{ exponent; default: \code{1}. }
}

\value{
The function \code{beta2tau} returns the \eqn{\tau} vector implied by \code{beta} and \code{distname}.

The function \code{beta_names} returns a list of names for each component of \code{beta}.
}
\author{Georg M. Goerg}
\examples{
beta2tau(c(1,1), distname="normal") # the same as the input, as delta = gamma = 0 and alpha = 1
beta2tau(c(1,4,1), distname="t") # problem (see message)
beta2tau(c(1,4,3), distname="t") # no problem
beta_names("normal")
beta_names("exp")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
