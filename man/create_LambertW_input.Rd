\name{create_LambertW_input}
\alias{create_LambertW_input}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Modular toolkit for generation of input distribution object}
\description{
(IMPORTANT: This functionality of the package is still in development; function names, functionality, etc. may be subject to change. If speed is your main concern, then use the individual function necessary to carry out your specific task; if convencience and functionality are important, then use this function and then \code{\link{create_LambertW_output}}.)

This function creates an object of class \code{LambertW_input} for standard distribution (see \code{\link{LambertW-package}} for a list of supported distributions) as well as user-defined distributions. This will then be passed on to \code{\link{create_LambertW_output}} to create the Lambert W version of the input.

See Details and Values below for instructions.
}
\usage{
create_LambertW_input(distname = NULL, beta = NULL, beta2tau = NULL, dU = NULL, 
    pU = NULL, rU = NULL, qU = NULL) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{distname}{ name of the input distribution \eqn{F_X(x \mid \boldsymbol \beta)}; default: \code{NULL}.}
  \item{beta}{ parameter vector \eqn{\boldsymbol \beta} of the input distribution \eqn{F_X(x \mid \boldsymbol \beta)}}
  \item{beta2tau}{ function to convert \eqn{\boldsymbol \beta}  to \eqn{\tau} for distribution \code{distname}.}
  \item{dU}{ function: probability density function (pdf) of the (zero-mean and) unit variance U version of \eqn{X \sim  F_X(x \mid \boldsymbol \beta)} (see References for details).}
  \item{pU}{ function: cumulative distribution function (cdf) of the (zero-mean and) unit variance U}
  \item{rU}{ function: random number generator for the (zero-mean and) unit variance U}
  \item{qU}{ function: quantile function for the (zero-mean and) unit variance U}
}

\details{
In a first step one generates a \code{LambertW_input} object representing the input RV \eqn{X \sim F_X(x \mid \boldsymbol \beta)}. In the second step it will be transformed to a Lambert W \eqn{\times} F distribution, where F can be any well-defined distribution - see \code{\link{create_LambertW_output}}. Standard distributions such as \code{"normal"}, \code{"t"} or \code{"exp"}onential are already implemented, but users can define any \code{LambertW_input} with their favorite distribution and then use \code{\link{create_LambertW_output}} to create a Lambert W \eqn{\times} 'favorite distribution' RV with the standard R functionality such as random number generation, pdf/cdf, quantiles, MLE, etc.\ for this newly generated Lambert W \eqn{\times} 'favorite distribution'. 

This modular toolkit implementation intends to promote a wide-spread usage of Lambert W \eqn{\times} F distributions in practice.

If \code{"distname"} is not one of the already implemented (see \code{\link{LambertW-package}} for a list of supported distributions), then \code{create_LambertW_input} uses \code{dU}, \code{rU}, etc.\ supplied by the user. For the correct format of these, type \code{create_LambertW_input} in the console and see the structure/format for the already implemented distributions; it should be self-explanatory.
}

\value{
A list of class \code{LambertW_input}; the values of an \code{LambertW_input} object are (for the most part) functions themselves (see Examples):
 \item{distname}{ name of the distribution \eqn{F_X(x \mid \boldsymbol \beta)}}
  \item{beta}{ parameter vector \eqn{\boldsymbol \beta} of the input distribution}
  \item{beta2tau}{ function to convert \eqn{\boldsymbol \beta} to the \eqn{\tau} }
  \item{tau}{tau vector}
 \item{dU}{ pdf of U}
 \item{pU}{ cdf of U}
 \item{qU}{ quantile function for U}
 \item{rU}{ random number generator for U}
 \item{dX}{ pdf of \eqn{X = U \cdot \sigma_x(\boldsymbol \beta) + \mu_x(\boldsymbol \beta)}}
 \item{pX}{ cdf of \eqn{X}}
 \item{qX}{ quantile function for X}
 \item{rX}{ random number generator for X}
}

\author{Georg M. Goerg}

\examples{
Gauss_input = create_LambertW_input("normal", beta = c(2,1))

Gauss_input$dX # this returns a function itself
Gauss_input$dX()(1) # evaluate the pdf of a N(2,1) at x = 1

plot(Gauss_input$dX(), -2,6) # by default it will take the beta from the LambertW_input object
plot(Gauss_input$dX(beta = c(3,4)), -2,6) # change the beta to c(3,4)

plot(Gauss_input$pX(), -3,6)
integrate(Gauss_input$dX(), -10,10)
Gauss_input$rX()(n = 100) # generate 100 samples from a N(2,1)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
\keyword{distribution}
\keyword{datagen}
