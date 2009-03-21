\name{AA}
\alias{AA}
\docType{data}
\title{Australian athletes data set}
\description{
  These data were collected in a study of how 
  data on various characteristics of the blood varied with sport
  body size and sex of the athlete. Equivalent to the \code{ais} dataset in the DAAG package.
}
\usage{data(AA)}
\format{
  A data frame with 202 observations on the following 13 variables.
  \describe{
    \item{rcc}{red blood cell count, in \eqn{10^{12} l^{-1}}{}}
    \item{wcc}{while blood cell count, in \eqn{10^{12}}{} per liter}
    \item{hc}{hematocrit, percent}
    \item{hg}{hemaglobin concentration, in g per decaliter}
    \item{ferr}{plasma ferritins, ng \eqn{dl^{-1}}{}}
    \item{bmi}{Body mass index, kg \eqn{cm^{-2} 10^2}{}}
    \item{ssf}{sum of skin folds}
    \item{pcBfat}{percent Body fat}
    \item{lbm}{lean body mass, kg}
    \item{ht}{height, cm}
    \item{wt}{weight, kg}
    \item{sex}{a factor with levels \code{f} \code{m}}
    \item{sport}{a factor with levels \code{B_Ball} \code{Field} 
\code{Gym} \code{Netball} \code{Row} \code{Swim} \code{T_400m} 
\code{T_Sprnt} \code{Tennis} \code{W_Polo}}
  }
}
\details{
  Do blood hemoglobin concentrations of athletes in endurance-related
  events differ from those in power-related events?
}
\source{
  These data were the basis for the analyses that are reported in
  Telford and Cunningham (1991).
}
\references{
Telford, R.D. and Cunningham, R.B. 1991.  Sex, sport and
  body-size dependency of hematology in highly trained athletes.
  Medicine and Science in Sports and Exercise 23: 788-794.
}
\keyword{datasets}


\examples{
data(AA)
attach(AA)
X=AA[AA$sex=="f",]
y=X$bmi

op=par(no.readonly=TRUE)
par(mfrow=c(2,1), mar=c(2,4,3,1))
plot(y)
hist(y, prob=TRUE, 15)
lines(density(y))
par(op)

fit.gmm=IGMM(y)
summary(fit.gmm) # Delta is significant and positive
plot(fit.gmm)
# Comparison of Theoretical and Empirical Moments
mom.LambertW.X.Gauss(fit.gmm$theta)
rbind(mean(y), sd(y), skewness(y), kurtosis(y))

x=get.input(y, fit.gmm$theta)$x

op=par(no.readonly=TRUE)
par(mfrow=c(2,1), mar=c(2,4,3,1))
plot(x)
hist(x, prob=TRUE, 15)
lines(density(x))
par(op)

plot(fit.gmm)
}


