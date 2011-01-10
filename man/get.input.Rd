\name{get.input}
\alias{get.input}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Back-transform Y to X}
\description{

 Given the data \code{y} and the parameter vector \eqn{\tau = (\mu_x(\boldsymbol \beta), \sigma_x(\boldsymbol \beta), \gamma, \alpha)} (or \eqn{\tau = (\mu_x(\boldsymbol \beta), \sigma_x(\boldsymbol \beta), \delta, \alpha)}), this function computes the back-transformed input data \eqn{\widehat{\boldsymbol x}_{\tau}}.
}
\usage{
get.input(y, tau, return.u=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a numeric vector of data values. }
  \item{tau}{ parameter vector }
  \item{return.u}{ should the normalized input be returned; default: \code{FALSE}}
}

\value{
The approximated input data vector \eqn{\widehat{\boldsymbol x}_{\tau}}.

For \code{gamma != 0} it uses the principal branch solution \code{\link{W_gamma}} to get a unique input. For \code{gamma = 0} the back-transformation is bijective (for any \eqn{\delta \geq 0}).

If \code{return.u = TRUE} then it returns a list with 2 vectors
 \item{u}{centered and normalized input u}
 \item{x}{input data \eqn{\widehat{\boldsymbol x}_{\theta}}}
}
\author{Georg M. Goerg}
\examples{
set.seed(12)

# unskew very skewed data
y=rLambertW(n=1000, beta=c(0,1), gamma = 0.3)
normfit(y)
fit.gmm=IGMM(y, type="s")
summary(fit.gmm)

x=get.input(y, fit.gmm$tau)
normfit(x) # symmetric Gaussian

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
