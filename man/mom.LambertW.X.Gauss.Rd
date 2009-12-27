\name{mom.LambertW.X.Gauss}
\alias{mom.LambertW.X.Gauss}
\alias{mom.LambertW.U.Gauss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Moments of Y for Gaussian Input}
\description{
Computes the first \eqn{4} central (normalized) moments of Y for Gaussian input \eqn{X \sim N(\mu_x, \sigma_x)} for a given \eqn{\theta}.

The special case of \eqn{X = U \sim N(0,1)} has its own function (in that case the notation changes \eqn{Y \rightarrow Z}.
}
\usage{
mom.LambertW.X.Gauss(theta = c(0, 0, 1))
mom.LambertW.U.Gauss(delta = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{ parameter vector }
  \item{delta}{skewness parameter}
}

\value{
A list containing the first \eqn{4} central (normalized) moments of Y
  \item{mu_y}{mean}
  \item{sigma_y}{standard deviation}
\item{skew}{Pearson skewness}
\item{kurt}{Pearson kurtosis}
If the input is \eqn{U} then the output names are
  \item{mu_z}{mean}
  \item{sigma_z }{standard deviation}
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
rbind(mean(y), sd(y), skewness(y), kurtosis(y)) # Sample values
mom.LambertW.X.Gauss(fit.gmm$theta) # Theoretical values
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
