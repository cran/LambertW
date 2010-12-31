\name{LambertW_input-methods}
\alias{print.LambertW_input}
\alias{plot.LambertW_input}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Methods for LambertW input objects}
\description{
\code{plot.LambertW_input} plots the theoretical (1) pdf and (2) cdf of the input \eqn{X \sim F_X(x \mid \boldsymbol \beta)} 

\code{print.LambertW_input} prints an overview of the generated object.
}
\usage{
\method{plot}{LambertW_input}(x, a = NULL, b = NULL, ...)
\method{print}{LambertW_input}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ object of class \code{LambertW_input} }
  \item{a}{ left limit from where cdf and pdf are plotted; not necessary to specify}
  \item{b}{ right limit until cdf and pdf are plotted; not necessary to specify}
  \item{\dots}{further arguments passed to or from other methods.}
}

\author{Georg M. Goerg}
\examples{
 # create a heavy-tailed version of a normal -> Tukey's h with h = 0.1
Gauss_input = create_LambertW_input("normal", beta = c(1,2))
Gauss_input
plot(Gauss_input)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{print}
