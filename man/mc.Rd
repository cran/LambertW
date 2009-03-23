\name{mc}
\alias{mc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{MedCouple Estimator}
\description{
Robust measure of skewness. 
}
\usage{
mc(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ data }
}

\value{
robust skewness measure
}
\references{Brys, G., M. Hubert, and A. Struyf (2004). \dQuote{A robust measure of skewness}. Journal of Computa-
tional and Graphical Statistics 13 (4), 996 - 1017.}
    
\author{Georg M. Goerg}


\examples{
x=rnorm(100)
mc(x)
y=rLambertW(n=100, theta=c(0.4,1,2))
mc(y)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
