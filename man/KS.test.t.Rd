\name{KS.test.t}
\alias{KS.test.t}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Kolmogorov - Smirnov test for student-t data}
\description{
Performs a KS test to see if data is student-t with nu degrees of freedom, mean \eqn{mu_x}, and stand. dev. \eqn{sigma_x}.
}
\usage{
KS.test.t(y, theta = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ data }
  \item{theta}{ theta incl. df parameter nu}
}

\value{
KS test output 
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
