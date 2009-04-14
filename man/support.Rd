\name{support}
\alias{support}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Truncated support}
\description{
Given \eqn{\theta} calculates the support of Y.
}
\usage{
support(theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{ parameter vector }
}

\value{
Half-open interval on the real line (if \eqn{\delta \neq 0})
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}

\examples{
support(c(0,0,1))

support(c(0.1,0,1))

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
