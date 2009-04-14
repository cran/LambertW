\name{W}
\alias{W}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Lambert W function (principal branch) }
\description{
Principal branch of the Lambert W function. This is a short name for \code{lambert_W0} in the package \code{gsl}.
}
\usage{
W(z)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ value }
}

\value{
Function value or NaN.
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished

Corless, R. M., G. H. Gonnet, D. E. G. Hare, and D. J. Jeffrey (1993). \dQuote{On the Lambert W function}. preprint.
}
\author{Georg M. Goerg}

\seealso{\code{\link[gsl]{lambert_W0}}}
\examples{
W(5) # exists
W(-5) # does not exist
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
