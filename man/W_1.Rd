\name{W_1}
\alias{W_1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Lambert W function (non-principal branch)}
\description{
Non-principal solution of the Lambert W function. Short for \code{lambert_W1} in the \code{gsl} package.
}
\usage{
W_1(z)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{value}
}

\value{
Function value or NaN
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished

Corless, R. M., G. H. Gonnet, D. E. G. Hare, and D. J. Jeffrey (1993). \dQuote{On the Lambert W function}. preprint.
}
\author{Georg M. Goerg}

\seealso{\code{\link[gsl]{lambert_W1}}}
\examples{
W(-0.25) # the "reasonable" input event
W_1(-0.25) # the "extreme" input event
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
