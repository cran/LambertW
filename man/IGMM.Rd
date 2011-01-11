\name{IGMM}
\alias{IGMM}
\alias{IGMM.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Iterative Generalized Method of Moments -- IGMM}
\description{
An iterative methods of moments estimator finds this \eqn{\tau = (\mu_x, \sigma_x, \gamma, 0)} (or \eqn{\tau = (\mu_x, \sigma_x,0, \delta)}) which minimizes the distance between the sample and theoretical skewness (or kurtosis) of \eqn{\boldsymbol x} and X. 

For details of the Algorithm see the References.
}
\usage{
IGMM(y, skewness_x = 0, type="s", kurtosis_x = 3, tau.0 = c(median(y), sd(y), (skewness(y) - 
    skewness_x)/6, max(0, (kurtosis(y)-3)/100)), robust = FALSE, tol = .Machine$double.eps^0.5, location_family = TRUE)
\method{IGMM}{default}(y, skewness_x = 0, type="s", kurtosis_x = 3, tau.0 = c(median(y), sd(y), (skewness(y) - 
    skewness_x)/6, max(0, (kurtosis(y)-3)/100)), robust = FALSE, tol = .Machine$double.eps^0.5, location_family = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a numeric vector of real values. }
  \item{skewness_x}{ theoretical skewness of input X; default \code{0}}
  \item{type}{ what type of distribution should be estimated? skewed Lambert W (\code{"s"}) or heavy-tail Lambert W (\code{"h"}) (the skewed Lambert W distribution (\code{"hh"}) is not implemented yet). }
  \item{kurtosis_x}{ theoretical kurtosis of input X; default \code{3} (Gaussian)}
  \item{tau.0}{ starting values for IGMM algorithm; default: \code{(median(y), sd(y), (skewness(y)-skewness_x)/6, max(0, (kurtosis(y)-3)/100))}}
\item{robust}{ indicator if robust estimation of the sample skewness (see \code{\link{mc}}) should be used; default: \code{FALSE} (only for \code{type = "s"})}
  \item{tol}{ a positive scalar giving the tolerance at which the distance is considered close enough to zero to terminate the algorithm; default: \code{.Machine$double.eps^0.5} }
  \item{location_family}{is the underlying input distribution \eqn{F_X(x \mid \boldsymbol \beta)} from a location family (for example, Gaussian input); default: \code{TRUE}. If \code{FALSE}, then \eqn{\theta_1 \equiv 0} throughout the optimization, for example for exponential input.}
}

\value{
A list of class \code{LambertW_fit}:
  \item{tol}{tolerance level; see Arguments for a detailed description}
  \item{data}{ data \code{y}}
  \item{n}{ number of observations}
  \item{type}{type of Lambert W distribution; see Arguments for a detailed description}
  \item{tau.0}{ starting value for \eqn{\theta} }
  \item{tau}{ IGMM estimate for \eqn{\theta} }
  \item{TAU}{entire iteration sequence of \eqn{\tau^{(k)}}}
  \item{sub_iterations}{number of iterations only performed in GMM algorithm to find optimal \eqn{\gamma} (or \eqn{\delta})}
  \item{iterations}{number of iterations performed by the IGMM algorithm to update \eqn{\mu_x} and \eqn{\sigma_x}. See References for detals.}
  \item{theta}{a list containing \eqn{\gamma} or \eqn{\delta} of IGMM.}
  \item{hessian}{ Hessian matrix (obtained from simulations; see References)}
  \item{call}{function call}
  \item{distname}{ a character string stating the theoretical skewness (or kurtosis) of the input distribution. Same information as \code{skewness_x} (or \code{kurtosis_x})}
  \item{skewness_x}{ theoretical skewness of the input (numeric value); default: \code{0}}
  \item{kurtosis_x}{ theoretical kurtosis of the input (numeric value); default: \code{3}}
  \item{location_family}{see Arguments above.}
  \item{message}{message from the optimization method. What kind of convergence?}
  \item{method}{Estimation method; here: \code{"IGMM"}}
}

\references{Goerg, G.M. (2011a). \dQuote{Lambert W Random Variables - A New Family of Generalized Skewed Distributions with Applications to Risk Estimation}. In review at the AOAS (arxiv.org/abs/0912.4554).}

\author{Georg M. Goerg}

\examples{
# estimate theta for the skewed version of a Normal
y=rLambertW(n=1000, beta = c(2,1), distname = "normal", gamma = 0.2)
fity=IGMM(y, type="s")
fity
summary(fity)
plot(fity)

# estimate theta for the heavy-tailed version of a Normal = Tukey's h
y=rLambertW(n=1000, beta = c(2,1), distname = "normal", delta = 0.2)
fity=IGMM(y, type="h")
fity
summary(fity)
plot(fity)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{iteration}
\keyword{optimize}
