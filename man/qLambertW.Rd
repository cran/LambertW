\name{qLambertW}
\alias{qLambertW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Quantile function}
\description{
Computes the quantiles given a probability level p.
}
\usage{
qLambertW(p, theta, distname = "normal")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{ vector of probability levels }
  \item{theta}{ parameter vector }
  \item{distname}{ input distribution }
}


\value{
Value(s) of quantile(s) that result(s) in probability level(s) p.
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}

\examples{

p.v=c(0.1, 0.5, 0.9, 0.95,0.99)

qnorm(p.v)
sapply(p.v, qLambertW, theta=c(0,0,1)) # should give the same as above

# positively skewed data -> quantiles are lower
qLambertW(p.v, theta=c(0.1,0,1))

qLambertW(p.v, theta=c(0.3,0,1))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
