\name{create_LambertW_output}
\alias{create_LambertW_output}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Modular toolkit for generation of output distribution object}
\description{
(IMPORTANT: This functionality of the package is still in development; function names, functionality, etc. may be subject to change.)

To use this function you have to use \code{\link{create_LambertW_input}} first.

This function takes an object of class \code{LambertW_input} and creates an object of class \code{LambertW_output} for standard distribution as well as the user-defined distribution. This \code{LambertW_output} represents the RV Y \eqn{\sim} Lambert W \eqn{\times} 'my favorite distribution' with all its properties and R functionality, such as random number generation (\code{rY}), pdf (\code{dY}) and cdf (\code{pY}), etc.\ for this newly generated Lambert W \eqn{\times} 'favorite distribution'. 

Please see Details and Values below for instructions.
}
\usage{
create_LambertW_output(input = NULL, theta = NULL, distname = input$distname)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{ an object of class \code{LambertW_input}}
  \item{theta}{ a list containing (not necessarily all) parameters (\code{alpha}, \code{beta}, \code{gamma}, \code{delta}). The function \code{\link{complete_theta}} fills in missing standard values for the parameters. If not provided, the function will print out a warning since the transformation is the identity and so the output RV Y \eqn{\equiv} input X.}
 \item{distname}{ name of the input distribution \eqn{F_X(x \mid \boldsymbol \beta)}; default: \code{input\$distname} }
}

\details{
This function converts a \code{LambertW_input} list into a \code{LambertW_output} list.

\code{create_LambertW_output} is based on the definition of Lambert W \eqn{\times} F RVs by the input/output approach (see References), and transports this very flexible and powerful framework to the code level. This allows users to create their own Lambert W \eqn{\times} 'my favorite distribution' RVs and then use R's standard functionality for distributions such as random number generation (\code{rY}), pdf (\code{dY}) and cdf (\code{pY}), quantile function (\code{qY}), etc.\ for this newly generated Lambert W \eqn{\times} 'favorite distribution' in empirical analysis. 

Thus users can start estimating parameters, estimate quantiles, plot density functions within minutes of a newly defined Lambert W \eqn{\times} F RV, which should promote a wide-spread use of Lambert W \eqn{\times} F distributions in practice.

If the distribution you inted to use is not one of the already implemented (\code{distname}), then 1) use \code{\link{create_LambertW_input}} with your favorite distribution, and 2) pass it as an input argument to the \code{create_LambertW_output} along with the parameters.
}

\value{
An object of class \code{LambertW_output}; the values of an \code{LambertW_output} object are (for the most part) functions themselves (see Examples):
 \item{dY}{ pdf of Y \eqn{\sim} Lambert W \eqn{\times} 'my favorite distribution' (see References)}
 \item{pY}{ cdf of Y}
 \item{rY}{ quantile function for Y}
 \item{rY}{ random number generator for Y}
 \item{distname}{ character string with the name of the new distribution. Format: "Lambert W x 'my favorite distribution'"}
  \item{beta}{ parameter vector \eqn{\boldsymbol \beta} of the input distribution \eqn{F_X(x \mid \boldsymbol \beta)}}
  \item{parameters}{ a list containing all parameters (\code{alpha}, \code{beta}, \code{gamma}, \code{delta}).}
  \item{theta}{ theta vector}
 \item{distname_with_beta}{name of the new distribution including the parameter \code{beta}. Format: "Lambert W x 'my favorite distribution(beta)'"}

}

\author{Georg M. Goerg}

\examples{
# create a Gaussian N(1,2) input
Gauss_input = create_LambertW_input("normal", beta = c(1,2))

# create a heavy-tailed version of a normal -> Tukey's h with h = 0.3
params = list(delta = c(0.3)) # gamma = 0, alpha = 1 will be assigned automatically if missing; beta comes from the input
LW.Gauss = create_LambertW_output(input = Gauss_input, theta = params)
LW.Gauss

op = par(no.readonly=TRUE)
par(mfrow=c(2,1), mar=c(3,3,2,1))
plot(LW.Gauss$dY(params),-7,10, col="red")
plot(LW.Gauss$dY(), -7, 10, col="blue") # same in blue; 
# actual information here: parameter for the specified model is not necessary to give again as input to the returned functions


params.0 = params # compare to the input case (i.e. set delta = 0)
params.0$delta = 0

plot(LW.Gauss$dY(params.0), -7,10, add=TRUE, col=1) # to evaluate the RV at a different parameter value, it is necessary to provide the new parameter in the function
par(op)

plot(LW.Gauss$pY(params), -7,10, col="red")
plot(LW.Gauss$pY(params.0), -7,10, add=TRUE, col=1)

normfit(LW.Gauss$rY(params)(n=100))

## generate a positively skewed version of the decentralized scaled t_3 distribution 
t_input = create_LambertW_input("t", beta = c(2,1,3))
t_input
params = list(gamma = 0.05) # skew it
LW.t = create_LambertW_output(input = t_input, theta = params)
LW.t

plot(LW.t$dY(params),-7,11, col = 2)
plot(t_input$dX(), -7,11, add=TRUE, col=1)
abline(v = params$beta[1])

# draw samples from the skewed t_3
yy = LW.t$rY()(n=100)
normfit(yy)

## generate a positively skewed version a Cauchy distribution
cauchy_input = create_LambertW_input("cauchy", beta = c(0,1))
params = list(alpha = 1, beta = cauchy_input$beta, gamma = 0.05, delta = 0) # the skewed version
LW.cauchy = create_LambertW_output(input = cauchy_input,distname=cauchy_input$distname, theta = params)
LW.cauchy


exp_input = create_LambertW_input("exp", beta = 1)
plot(exp_input)

# create a skewed exponential distribution
params = list("gamma" = 0.2)
LW.exp = create_LambertW_output(exp_input, theta = params)
plot(LW.exp)

# create a heavy-tail exponential distribution
params = list("delta" = 0.2)
LW.exp = create_LambertW_output(exp_input, theta = params)
plot(LW.exp)

# create a skewed chi-square distribution
chi_input = create_LambertW_input("chisq", beta = 5) # a Chi^2 with 5 df
plot(chi_input)
params = list("gamma" = sqrt(2)*0.2)
LW.chi = create_LambertW_output(chi_input, theta = params)
plot(LW.chi)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
\keyword{distribution}
\keyword{datagen}
\keyword{models}
