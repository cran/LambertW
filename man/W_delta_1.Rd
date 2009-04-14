\name{W_delta_1}
\alias{W_delta_1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Inverse transformation (non-principal)}
\description{
Inverse transformation for Lambert W RVs. This function uses the non-principal branch.
}
\usage{
W_delta_1(z, delta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ value }
  \item{delta}{ skewness parameter }
}

\value{
Computes the value \eqn{1/\delta * W_[1](\delta z)}.
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
