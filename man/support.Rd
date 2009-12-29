\name{support}
\alias{support}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Truncated support}
\description{
Computes the support \eqn{[a,b]} (including \eqn{a,b = \pm \infty}) of Y given \eqn{\theta}.
}
\usage{
support(theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{ parameter vector }
}

\value{
Half-open interval on the real line (if \eqn{\delta \neq 0}). For \eqn{\delta = 0} the support is the same as for the input distribution (hence \eqn{(-\infty, \infty)} for Gaussian and student-t input).
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}

\examples{
support(c(0,0,1)) # as delta = 0
support(c(0.1,0,1)) # truncated

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
