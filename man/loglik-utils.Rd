\name{loglik-utils}
\alias{loglik_input}
\alias{loglik_penalty}
\alias{loglik_LambertW}
\alias{theta2params}
\alias{params2theta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Log-Likelihood of the data for Lambert W x F_X RVs}
\description{
The log-likelihood of Lambert W \eqn{\times} \eqn{F} RVs in most cases decomposes into two additive terms: i) the log-likelihood of the input, and ii) a penalty term for transforming the data.

\code{loglik_input} computes the log-likelihood for data \code{x} for various distributions given the parameter vector \eqn{\boldsymbol \beta}. It can therefore be used independently of the Lambert W framework, just in general to compute log-likelihood of common distributions and parameters.

\code{loglik_penalty} computes the penalt of transforming the data back to the input (not necessaril Gaussian; any distribution specified by \code{distname}). 

\code{loglik_LambertW} simply adds the two values to get the actual log-likelihood of the data \code{y} under Lambert W \eqn{\times} \eqn{F} distributions and parameter vector \eqn{\theta}.

\code{theta2params} and \code{params2theta} are internal functions that are used to simplify the conversion between the list-type \code{theta} and the vector-style argument of all optimization routines (which all require a vector-type argument of the function to be optimized).
}
\usage{
loglik_input(beta = NULL, x = NULL, distname = "normal", f_X = NULL, 
logf_X = function(x) log(f_X(x)))
loglik_penalty(theta, y = NULL, type = "h", distname = "normal")
theta2params(theta)
params2theta(params, distname="normal", type ="h")
loglik_LambertW(theta, y = NULL, distname = "normal", type = "h", 
return.neg.value = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a numeric vector of real values (the observed data). }
  \item{x}{ a numeric vector of real values (the 'input' data). Can be used also for general data values to evaluate the log-likelihood (not necessaril in the Lambert W framework).}
    \item{distname}{input distribution. default \code{"normal"}.}
    \item{beta}{ parameter vector \eqn{\boldsymbol \beta} for the input distribution. Please see specifications in the code how to write \code{beta} for each distribution.}
  \item{f_X}{ density function of \code{x}. Common distributions are already built-in (see \code{distname}). If you want to supply your own density, ou must supply a function of \code{x} and \code{beta}.}
  \item{logf_X}{a function that returns the logarithm of the density function of \code{x}. Often the \eqn{\log} of \eqn{f_X(x)} has a simpler form (which is also faster to evaluate).}
    \item{theta}{ the parameter vector in form of a list containing \code{beta}, \code{gamma}, \code{delta} etc. }
  \item{type}{ what type of transformation? skewed Lambert W \code{"s"}; heavy-tail Lambert W \code{"h"}; heavy-tails Lambert W \code{"hh"}. }
  \item{params}{a vector containg the parameter values of \eqn{\theta} in an unlisted way.}
    \item{return.neg.value}{ an indicator to return only the negative log-likelihood of the data (which is useful for optimization routines); otherwise it returns the list of input, penalty, and their sum = full likelihood. Default: \code{FALSE}.}
}
\details{
See the references for the detailed expressions of log-likelihood and penalty terms.

Generally, for the heavy-tail versions (\code{type = "h"} or \code{type = "hh"}) such a decomposition always exists. For skewed Lambert W distributions it only exists for non-negative input (e.g.\ exponential, gamma, F, \ldots), not (!) for \code{"normal", "t", "unif", "cauchy", ...}. In this case \code{loglik_input} and
\code{loglik_penalty} will return \code{NA}, but the full log-likelihood will be returned correctly.
}

\value{
\code{loglik_input} and \code{loglik_penalty} return one value.

\code{loglik_LambertW} 
Returns a list with 3 values:
  \item{loglik_input}{ the loglikelihood of the transformed variable}
  \item{loglik_penalty}{the penalty for transforming the data}
  \item{loglik_LambertW}{the total log-likelihood of the data given \eqn{\theta} = sum of the two values before.}

}
\references{Goerg, G.M. (2011a). \dQuote{Lambert W Random Variables - A New Family of Generalized Skewed Distributions with Applications to Risk Estimation}. Forthcoming in the Annals of Applied Statistics (arxiv.org/abs/0912.4554).

Goerg, G.M. (2011b). \dQuote{The Lambert Way to Gaussianize skewed, heavy-tailed data with the inverse of Tukey's h transformation as a special case.}. In preparation for submission (http://arxiv.org/abs/1010.2265).
}
\author{Georg M. Goerg}

\examples{
###
set.seed(1)
yy = rLambertW(n = 1000, beta = c(0,1), delta = 0.2)
loglik_penalty(list(beta = c(1,1), delta = c(0.2,0.2)), y = yy, 
              type = "hh", distname = "normal")
# here such a decomposition doesn't exist; thus NA
loglik_penalty(list(beta = c(1,1), delta = c(0.2), gamma = 0.03), y = yy, 
               type = "h", distname = "normal") 

### computing the Gaussian log-likelihood
loglik_input(beta = c(0,1), x = yy, distname = "normal") # built-in version

 # or defe it yourself
loglik_input(beta = c(0,1), x = yy, distname = "user", 
             logf_X = function(xx, beta = beta) 
                   dnorm(xx, mean = beta[1], sd = beta[2], log = TRUE))

### loglik_LambertW
loglik_LambertW(theta = list(beta = c(1,1), delta = c(0.09,0.07)), 
                y = yy, type = "hh", distname ="normal") # all three

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
\keyword{distribution}
\keyword{datagen}
