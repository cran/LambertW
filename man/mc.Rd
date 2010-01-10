\name{mc}
\alias{mc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{MedCouple Estimator}
\description{
A robust measure of asymmetry. Details see references.
}
\usage{
mc(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ data }
}

\value{
a numerical value, indicating the amount of asymmetry
}
\references{Brys, G., M. Hubert, and A. Struyf (2004). \dQuote{A robust measure of skewness}. Journal of Computa-
tional and Graphical Statistics 13 (4), 996 - 1017.}
    
\author{Georg M. Goerg}

\examples{
## a little simulation/demonstration
n.sim = 100

################# Gaussian (Symmetric) #### 
A = cbind(0,0)
for (i in 1:n.sim) {
x = rnorm(100)
A = rbind(A, c(mc(x), skewness(x)))
}
A = A[-1,]

################# Gaussian (Symmetric) #### 
theta.s = delta.01(0.2) # zero mean, unit variance, but positive skewness
round(mLambertW(theta.s),5)

B = cbind(0,0)
for (i in 1:n.sim) {
y = rLambertW(n=100, theta=theta.s)
B = rbind(B, c(mc(y), skewness(y)))
}
B = B[-1,]

colnames(A) = colnames(B) = c("MedCouple", "Pearson Skewness")
op=par(no.readonly=TRUE)

par(mfrow=c(2,2), mar=c(4,4,3,1))
plot(A, main="Gaussian")
boxplot(A)
abline(h=0)

plot(B, main="Lambert W -Gaussian")
boxplot(B)
abline(h=mLambertW(theta.s)[3])
par(op)

apply(A, 2, mean)
apply(A, 2, sd)

apply(B, 2, mean)
apply(B, 2, sd)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
