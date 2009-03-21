\name{summary.LWest}
\alias{summary.LWest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summary of Lambert W estimation}
\description{
Summary of a Lambert W estimation (GMM or ML).
}
\usage{
\method{summary}{LWest}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ object of class \code{LWest}}
  \item{\dots}{...}
}

\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{call}{Description of 'comp1'}
  \item{coefmat}{estimate matrix with estimates, stand. err., t-values and p-values}
 \item{distname}{distribution}
  \item{observations}{number of observations}
  \item{input}{back-transformed data}
  \item{support}{support of Y}
  \item{data.range}{original data range}
  \item{method}{Estimation method used.}
  \item{p_1}{Probability for the non-principal branch} 
}
\references{Goerg, G.M. (2009). \dQuote{Lambert W Random Variables - A new class of skewed distribution functions}. Unpublished}
\author{Georg M. Goerg}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print}
