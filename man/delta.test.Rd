\name{delta.test}
\alias{delta.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Test for significance of delta}
\description{
Given the simulated critical values for the GMM method and for delta=0 (see Goerg (2009)), this function performs a test on the significance an estimated delta.
}
\usage{
delta.test(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{object of class \code{LWest}}
}

\details{
If the data is truly Gaussian -- hence true \eqn{\delta = 0} -- then

\deqn{
\frac{\widehat{\delta} \sqrt{N}}{0.4} \sim N(0,1),
}
where N is the sample size, and \eqn{\widehat{\delta}} is the estimated delta via GMM.
}

\value{
  A list with class "htest" containing the following components:
  \item{statistic}{the value of the test statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string "Simulation based GMM significanc test (Asymptotic Gaussian)"}
  \item{data.name}{a character string giving the name(s) of the data.}
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}

\examples{
set.seed(1)
y=rnorm(1000)
fit=IGMM(y)
delta.test(fit) # not-significant because data is truly Gaussian

data(AA)
attach(AA)
X=AA[AA$sex=="f",]
y=X$bmi

fit=IGMM(y)
delta.test(fit) ### Significant delta as the data is highly skewed to the right
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
