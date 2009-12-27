\name{mc}
\alias{mc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{MedCouple Estimator}
\description{
A robust measure of asymmetry.
}
\usage{
mc(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ data }
}

\value{
The robust measure of skewness. Details see references.
}
\references{Brys, G., M. Hubert, and A. Struyf (2004). \dQuote{A robust measure of skewness}. Journal of Computa-
tional and Graphical Statistics 13 (4), 996 - 1017.}
    
\author{Georg M. Goerg}


\examples{
x=rnorm(1000)
mc(x)
y=rLambertW(n=1000, theta=c(0.4,1,2))
mc(y)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
