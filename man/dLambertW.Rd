\name{dLambertW}
\alias{dLambertW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Probability Density Function}
\description{
Computes the probability density of Y for Gaussian and student-t input.
}
\usage{
dLambertW(y, theta = c(0, 0, 1), distname = c("normal"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ value }
  \item{theta}{ parameter vector }
  \item{distname}{ input distribution }
}

\value{
Value of the ddf at y.
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}
\examples{
theta.s=c(0.1,0,1)

aux.lambert=function(y){
dLambertW(y, theta=theta.s)
}

support(theta.s)

plot(aux.lambert, -3.5,3.5, ylab="Probability")
plot(dnorm, -3.5,3.5, add=TRUE, lty=2)
legend("topleft" , c("Lambert W - Gaussian" , "Gaussian"), lty=1:2)

#############
data(AA)
attach(AA)
X=AA[AA$sex=="f",]
y=X$bmi

fit.gmm=IGMM(y)
summary(fit.gmm) # Delta is significant and positive
plot(fit.gmm)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
