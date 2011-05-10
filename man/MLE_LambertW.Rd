\name{MLE_LambertW}
\alias{MLE_LambertW}
\alias{MLE_LambertW.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Maximum Likelihood Estimation for Lambert W x F distributions}
\description{
Maximum Likelihood Estimation (MLE) for Lambert W \eqn{\times F_X(\cdot \mid \boldsymbol \beta)} distributions; computes 
\eqn{\widehat{\theta}_{MLE}}.

For \code{type = "s"} the paramter \eqn{\gamma} is estimated and \eqn{\delta = 0} is held fixed; for \code{type = "h"} the one-dimensional \eqn{\delta} is estimated and \eqn{\gamma = 0} is held fixed; and finally for \code{type = "hh"} the 2-dimensional \eqn{\delta} is estimated and \eqn{\gamma = 0} is held fixed.

By default \eqn{\alpha = 1} is fixed for any \code{type}. If you want to also estimate \eqn{\alpha} from the data pass \code{fixed_theta = list()} to \code{MLE_LambertW}.
}
\usage{
MLE_LambertW(y, distname = c("normal"), type = "s", theta.0 = list(), fixed_theta = list(alpha = 1), hessian = TRUE, estimate_only = FALSE) 
\method{MLE_LambertW}{default}(y, distname = c("normal"), type = "s", theta.0 = list(), fixed_theta = list(alpha = 1), hessian = TRUE, estimate_only = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a numeric vector of real values. }
  \item{distname}{ input distribution; default: \code{"normal"}.}
  \item{theta.0}{ a list containing the starting values of \eqn{(\alpha, \boldsymbol \beta, \gamma, \delta)} for the numerical optimization; default: see \code{\link{starting_theta}}.}
  \item{hessian}{ indicator for returning the (numerically obtained) Hessian at the optimum; default: \code{TRUE}.}
  \item{fixed_theta}{ a list of fixed parameters in the optimization; default only \code{alpha = 1}.}
\item{type}{ what type of distribution should be estimated? skewed Lambert W \code{"s"}; heavy-tail Lambert W \code{"h"}; heavy-tails Lambert W \code{"hh"}. }
\item{estimate_only}{ an indicator if only \eqn{\widehat{\theta}_{MLE}} should be returned (or the entire output); for simulations, for example, it is not necessary to give a nicely formatted, organized output, because the only interest lies in \eqn{\widehat{\theta}_{MLE}}; default: \code{FALSE}.}
}

\value{
A list of class \code{LambertW_fit}:
 \item{data}{ the data \code{y}}
  \item{loglik}{function: log-likelihood function}
  \item{loglik.opt}{value: log-likelihood evaluated at the optimum}
  \item{theta.0}{a list conatining the starting values for numerical optimization}
  \item{parameters}{ a list containing the estimated values of \eqn{(\alpha, \boldsymbol \beta, \gamma, \delta)}}
  \item{beta}{ estimated \eqn{\boldsymbol \beta} of the input distribution via Lambert W MLE (\eqn{\neq \widehat{\boldsymbol \beta}} of the input data )}
  \item{theta}{MLE for \eqn{\theta}  }
  \item{type}{see Arguments above}
 \item{hessian}{Hessian matrix; used to calculate standard errors (only if \code{hessian = TRUE}, otherwise \code{NULL})} 
\item{call}{function call}
 \item{distname}{character string describing the input distribution.}
 \item{message}{message from the optimization method. What kind of convergence?}
 \item{method}{Estimation method; here \code{"MLE"}}

}

\author{Georg M. Goerg}

\examples{
data(AA)
attach(AA)

X=AA[AA$sex=="f",]
y=X$bmi

fit.ml=MLE_LambertW(y, type="s")    # skewed only
summary(fit.ml)
plot(fit.ml)

fit.ml = MLE_LambertW(y, type="hh") # skewed heavy-tails (not too good fit because delta_l = 0)
plot(fit.ml)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{optimize}
