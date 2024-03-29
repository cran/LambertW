% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Gaussianize.R
\name{Gaussianize}
\alias{Gaussianize}
\title{Gaussianize matrix-like objects}
\usage{
Gaussianize(
  data = NULL,
  type = c("h", "hh", "s"),
  method = c("IGMM", "MLE"),
  return.tau.mat = FALSE,
  inverse = FALSE,
  tau.mat = NULL,
  verbose = FALSE,
  return.u = FALSE,
  input.u = NULL
)
}
\arguments{
\item{data}{a numeric matrix-like object; either the data that should be
Gaussianized; or the data that should ''DeGaussianized'' (\code{inverse =
TRUE}), i.e., converted back to the original space.}

\item{type}{what type of non-normality: symmetric heavy-tails \code{"h"}
(default), skewed heavy-tails \code{"hh"}, or just skewed \code{"s"}.}

\item{method}{what estimator should be used: \code{"MLE"} or \code{"IGMM"}.
\code{"IGMM"} gives exactly Gaussian characteristics (kurtosis
\eqn{\equiv} 3 for \code{"h"} or skewness \eqn{\equiv} 0 for \code{"s"}),
\code{"MLE"} comes close to this. Default: \code{"IGMM"} since it is much
faster than \code{"MLE"}.}

\item{return.tau.mat}{logical; if \code{TRUE} it also returns the estimated
\eqn{\tau} parameters as a matrix (same number of columns as
\code{data}).  This matrix can then be used to \code{Gaussianize} new
data with pre-estimated \eqn{\tau}. It can also be used to
``DeGaussianize'' data by passing it as an argument (\code{tau.mat}) to
\code{Gaussianize()} and set \code{inverse = TRUE}.}

\item{inverse}{logical; if \code{TRUE} it performs the inverse transformation
using \code{tau.mat} to "DeGaussianize" the data back to the original
space again.}

\item{tau.mat}{instead of estimating \eqn{\tau} from the data you can pass it
as a matrix (usually obtained via \code{Gaussianize(..., return.tau.mat =
TRUE)}). If \code{inverse = TRUE} it uses this \code{tau} matrix to
``DeGaussianize'' the data again.  This is useful to back-transform new
data in the Gaussianized space, e.g., predictions or fits, back to the
original space.}

\item{verbose}{logical; if \code{TRUE}, it prints out progress information in
the console. Default: \code{FALSE}.}

\item{return.u}{logical; if \code{TRUE} it returns the zero-mean, unit
variance Gaussian input.  If \code{FALSE} (default) it returns the input
\eqn{X}.}

\item{input.u}{optional; if you used \code{return.u = TRUE} in a previous
step, and now you want to convert the data back to original space, then
you have to pass it as \code{input.u}.  If you pass numeric data as
\code{data}, \code{Gaussianize} assumes that \code{data} is the input
corresponding to \eqn{X}, not \eqn{U}.}
}
\value{
numeric matrix-like object with same dimension/size as input \code{data}. 
If \code{inverse = FALSE} it is the Gaussianize matrix / vector; 
if \code{TRUE} it is the ``DeGaussianized'' matrix / vector.

The numeric parameters of mean, scale, and skewness/heavy-tail parameters
    that were used in the Gaussianizing transformation are returned as
    attributes of the output matrix: \code{'Gaussianized:mu'},
    \code{'Gaussianized:sigma'}, and for

\item{type = "h":}{\code{'Gaussianized:delta'} & \code{'Gaussianized:alpha'},}
\item{type = "hh":}{\code{'Gaussianized:delta_l'} and \code{'Gaussianized:delta_r'} & 
\code{'Gaussianized:alpha_l'} and \code{'Gaussianized:alpha_r'},}
\item{type = "s":}{\code{'Gaussianized:gamma'}.}

They can also be returned as a separate matrix using \code{return.tau.mat =
    TRUE}. In this case \code{Gaussianize} returns a list with elements:
    \item{input}{Gaussianized input data \eqn{\boldsymbol x} (or
    \eqn{\boldsymbol u} if \code{return.u = TRUE}),} \item{tau.mat}{matrix
    with \eqn{\tau} estimates that we used to get \code{x}; has same number
    of columns as \code{x}, and 3, 5, or 6 rows (depending on
    \code{type='s'}, \code{'h'}, or \code{'hh'}).}
}
\description{
\code{Gaussianize} is probably the most useful function in this package. It
    works the same way as \code{\link[base]{scale}}, but instead of just
    centering and scaling the data, it actually \emph{Gaussianizes} the data
    (works well for unimodal data).  See Goerg (2011, 2016) and Examples.

\strong{Important:} For multivariate input \code{X} it performs a column-wise
    Gaussianization (by simply calling \code{apply(X, 2, Gaussianize)}),
    which is only a marginal Gaussianization.  This does \emph{not} mean (and
    is in general definitely not the case) that the transformed data is then
    jointly Gaussian.

By default \code{Gaussianize} returns the \eqn{X \sim N(\mu_x, \sigma_x^2)}
    input, not the zero-mean, unit-variance \eqn{U \sim N(0, 1)} input.  Use
    \code{return.u = TRUE} to obtain \eqn{U}.
}
\examples{

# Univariate example
set.seed(20)
y1 <- rcauchy(n = 100)
out <- Gaussianize(y1, return.tau.mat = TRUE)
x1 <- get_input(y1, c(out$tau.mat[, 1]))  # same as out$input
test_normality(out$input) # Gaussianized a Cauchy!

kStartFrom <- 20
y.cum.avg <- (cumsum(y1)/seq_along(y1))[-seq_len(kStartFrom)]
x.cum.avg <- (cumsum(x1)/seq_along(x1))[-seq_len(kStartFrom)]

plot(c((kStartFrom + 1): length(y1)), y.cum.avg, type="l" , lwd = 2, 
     main="CLT in practice", xlab = "n", 
     ylab="Cumulative sample average", 
     ylim = range(y.cum.avg, x.cum.avg))
lines(c((kStartFrom+1): length(y1)), x.cum.avg, col=2, lwd=2)
abline(h = 0)
grid()
legend("bottomright", c("Cauchy", "Gaussianize"), col = c(1, 2), 
       box.lty = 0, lwd = 2, lty = 1)

plot(x1, y1, xlab="Gaussian-like input", ylab = "Cauchy - output")
grid()
\dontrun{
# multivariate example
y2 <- 0.5 * y1 + rnorm(length(y1))
YY <- cbind(y1, y2)
plot(YY)

XX <- Gaussianize(YY, type = "hh")
plot(XX)

out <- Gaussianize(YY, type = "h", return.tau.mat = TRUE, 
                   verbose = TRUE, method = "IGMM")
                   
plot(out$input)
out$tau.mat

YY.hat <- Gaussianize(data = out$input, tau.mat = out$tau.mat,
                      inverse = TRUE)
plot(YY.hat[, 1], YY[, 1])
}

}
\keyword{multivariate}
\keyword{univar}
