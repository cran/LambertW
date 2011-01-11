\name{LambertW_output-methods}
\alias{plot.LambertW_output}
\alias{print.LambertW_output}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Methods for LambertW output objects}
\description{
\code{plot.LambertW_output} plots the theoretical (1) pdf and (2) cdf of the output RV \eqn{Y \sim} Lambert W \eqn{\times} \eqn{F_X(x \mid \boldsymbol \beta)}. It also adds the plot of the input RV  \eqn{X \sim F_X(x \mid \boldsymbol \beta)} with no transformation (\eqn{\gamma = \delta = 0, \alpha = 1}).

\code{print.LambertW_output} prints an overview of the generated object.
}
\usage{
\method{plot}{LambertW_output}(x, a = NULL, b = NULL, ...)
\method{print}{LambertW_output}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ object of class \code{LambertW_output} }
  \item{a}{ left limit from where cdf and pdf are plotted; not necessary to specify}
  \item{b}{ right limit until cdf and pdf are plotted; not necessary to specify}
  \item{\dots}{further arguments passed to or from other methods.}
}

\author{Georg M. Goerg}
\examples{
 # create a heavy-tailed version deta = 0.3 of a Gaussian -> Tukey's h with h = 0.3
Gauss_input = create_LambertW_input("normal", beta = c(1,2))
params = list(delta = c(0.3))
LW.Gauss = create_LambertW_output(input = Gauss_input, theta = params)
LW.Gauss # print 
plot(LW.Gauss)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{print}
