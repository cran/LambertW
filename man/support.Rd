\name{support}
\alias{support}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Truncated support}
\description{
Computes the theoretical support \eqn{[a,b]}, \eqn{a,b \in R \cup \pm \infty} of Y given \eqn{\theta}.
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
support(c(0.1,0,1)) # truncated on the left, as delta >0

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
