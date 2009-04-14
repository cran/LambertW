\name{mom.LambertW.X.Gauss}
\alias{mom.LambertW.X.Gauss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Moments of Y for Gaussian Input}
\description{
Computes the first 4 moments of Y given theta and Gaussian input
}
\usage{
mom.LambertW.X.Gauss(theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{ parameter vector }
}

\value{
First four moments of Y
  \item{mu_y}{mean}
  \item{sigma_y}{stand. dev.}
\item{skew}{Pearson skewness}
\item{kurt}{Pearson kurtosis}
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}

\examples{
data(AA)
attach(AA)
X=AA[AA$sex=="f",]
y=X$bmi

fit.gmm=IGMM(y)
summary(fit.gmm) # Delta is significant and positive
plot(fit.gmm)
# Comparison of Theoretical and Empirical Moments
mom.LambertW.X.Gauss(fit.gmm$theta)
rbind(mean(y), sd(y), skewness(y), kurtosis(y))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
