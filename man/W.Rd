\name{W}
\alias{W}
\alias{W_1}
\alias{d1W}
\alias{d1W_1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Lambert W function and derivative }
\description{
The Lambert W function \code{W(z), W_1(z)} and its first derivative (\code{d1W(z), d1W_1(z)}). For both the principal and non-principal branch. (These are wrapper functions for \code{lambert_W0, lambert_Wm1} in the package \code{gsl}.)
}
\usage{
W(z)
W_1(z)
d1W(z)
d1W_1(z)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ a numeric vector of real values. }
}

\details{
The Lambert W function \eqn{W(z)} is implicitly defined as the inverse function of 
\deqn{
W(z) \exp(W(z)) = z.
}

For \eqn{z \geq 0} the solution is unique \code{W(z)}.

For \eqn{-1/e \leq z < 0} it has two solutions: the principal (\code{W(z)}) and non-principal branch (\code{W_1(z)}).

For \eqn{z  < -1/e} no solution exists in the reals.


The derivative can be expressed as a function of \eqn{W(z)}:
\deqn{
W'(z) = \frac{1}{(1 + W(z)) \exp(W(z)} =  \frac{W(z)}{z(1 + W(z))}, \quad z \neq 0, -1/e.
}

For details see the references.
}

\value{
Function value or NaN if \eqn{z < -1/e}.
}
\references{
Corless, R. M., G. H. Gonnet, D. E. G. Hare, and D. J. Jeffrey (1993). \dQuote{On the Lambert W function}. preprint.
}
\author{Georg M. Goerg}

\seealso{\code{\link[gsl]{lambert_W0}}, \code{\link[gsl]{lambert_Wm1}}}
\examples{
W(5) # exists in R
W(-5) # does not exist in R

W(-0.25) # the "reasonable" input event
W_1(-0.25) # the "extreme" input event
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
