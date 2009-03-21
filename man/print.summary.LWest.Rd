\name{print.summary.LWest}
\alias{print.summary.LWest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summary of estimation }
\description{
 Gives a summary of a Lambert W estimation. (GMM or ML)
}
\usage{
\method{print}{summary.LWest}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ object of class \code{LWest} }
  \item{\dots}{...}
}

\value{
Output with estimation results and useful info for Y implied by this model.
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}


\examples{
x=rLambertW(200, theta=c(0.1, 0, 1))
fit=IGMM(x)
summary(fit)

fitl=MLE_LambertW(x)
summary(fitl)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print}
