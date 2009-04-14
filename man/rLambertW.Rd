\name{rLambertW}
\alias{rLambertW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Random Generation of Lambert W RV}
\description{
Random generation of Lambert W RV
}
\usage{
rLambertW(n, theta = c(0, 0, 1), distname = "normal", innov = NULL, ZmUv = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ number of observations}
  \item{theta}{ parameter vector }
  \item{distname}{distribution}
  \item{innov}{pre-specified input. optional.}
  \item{ZmUv}{ Should the mean and stand. dev. of X be chosen such that Y is a zero mean, unit variance RV. default \code{FALSE}}
}
\details{
If the input data is given via \code{innov}, then the theoretical mean and standard deviation are replace by the empirical counterparts.
}
\value{
data vector y
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}

\examples{
set.seed(1)
x=rnorm(1000)
skewness(x) #almost no skewness
mc(x) # close to zero

y=rLambertW(n=1000, c(0.5,0,1))
mom.LambertW.X.Gauss(c(0.5,0,1))
skewness(y) # high positive skewness (in theory equal to 3.70)
mc(y) # also the robust measure gives a high value


op=par(no.readonly=TRUE)
par(mfrow=c(2,2), mar=c(2,4,3,1))
plot(x)
hist(x, prob=TRUE, 15)
lines(density(x))


plot(y)
hist(y, prob=TRUE, 15)
lines(density(y))
par(op)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
