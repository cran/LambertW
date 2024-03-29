% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze_convergence.R
\name{analyze_convergence}
\alias{analyze_convergence}
\alias{summary.convergence_LambertW_fit}
\alias{plot.convergence_LambertW_fit}
\title{Analyze convergence of Lambert W estimators}
\usage{
analyze_convergence(
  LambertW_fit,
  sample.sizes = round(seq(0.2, 1, length = 5) * length(LambertW_fit$data)),
  ...
)

\method{summary}{convergence_LambertW_fit}(object, type = c("basic", "norm", "perc", "bca"), ...)

\method{plot}{convergence_LambertW_fit}(x, ...)
}
\arguments{
\item{LambertW_fit, object, x}{an object of class \code{"LambertW_fit"} with an
\code{IGMM} or \code{MLE_LambertW} estimate.}

\item{sample.sizes}{sample sizes for several steps of the convergence
analysis.  By default, one of them equals the length of the original
data, which leads to improved plots (see
\code{\link{plot.convergence_LambertW_fit}}); it is not necessary,
though.}

\item{...}{additional arguments passed to \code{\link{bootstrap}} or
\code{\link[boot]{boot.ci}} in \pkg{boot} package.}

\item{type}{type of confidence interval from bootstrap estimates. Passes this
argument along to \code{\link[boot]{boot.ci}}.  However, contrary to 
the \code{type} argument in \code{\link[boot]{boot.ci}}, the \code{summary}
function can only take one of \code{c("basic", "norm", "perc", "bca")}.
See \code{\link[boot]{boot.ci}} for details.}
}
\description{
Analyzes the feasibility of a Lambert W x F distribution for a
    given dataset based on bootstrapping.  In particular it checks whether
    parameter estimates support the hypothesis that the data indeed follows a
    Lambert W x F distribution with finite mean and variance of the input
    distribution, which is an implicit assumption of Lambert W x F random
    variables in Goerg (2011).

See Goerg (2016) for an alternative definition that does not rely on fnite
second order moments (set \code{use.mean.variance = FALSE} to use that type
of Lambert W \eqn{\times} F distributions).
}
\details{
Stehlik and Hermann (2015) show that when researchers use the IGMM algorithm
    outlined in Goerg (2011) erroneously on data that does not have finite
    input variance (and hence mean), the algorithm estimates do not converge.

In practice, researchers should of course first check if a given model is
appropriate for their data-generating process.  Since original Lambert W x F
distributions assume that mean and variance are finite, it is not a given
that for a specific dataset the Lambert W x F setting makes sense.

The bootstrap analysis reverses Stehlik and Hermann's argument and checks
    whether the IGMM estimates \eqn{\lbrace \hat{\tau}^{(n)} \rbrace_{n}}
    converge for increasing (bootstrapped) sample size \eqn{n}: if they do,
    then modeling the data with a Lambert W x F distribution is appropriate;
    if estimates do not converge, then this indicates that the input data is
    too heavy tailed for a classic skewed location-scale Lambert W x F
    framework. In this case, take a look at (double-)heavy tailed Lambert W x
    F distributions (\code{type = 'hh'}) or unrestricted location-scale
    Lambert W x F distributions (\code{use.mean.variance = FALSE}). For
    details see Goerg (2016).
}
\examples{
\dontrun{

sim.data <- list("Lambert W x Gaussian" = 
                    rLambertW(n = 100, distname = "normal", 
                              theta = list(gamma = 0.1, beta = c(1, 2))),
                 "Cauchy" = rcauchy(n = 100))
# do not use lapply() as it does not work well with match.call() in
# bootstrap()
igmm.ests <- list()
conv.analyses <- list()
for (nn in names(sim.data)) {
  igmm.ests[[nn]] <- IGMM(sim.data[[nn]], type = "s")
  conv.analyses[[nn]] <- analyze_convergence(igmm.ests[[nn]])
}
plot.lists <- lapply(conv.analyses, plot)
for (nn in names(plot.lists)) {
  plot.lists[[nn]] <- lapply(plot.lists[[nn]], "+", ggtitle(nn))
}

require(gridExtra)
for (jj in seq_along(plot.lists[[1]])) {
  grid.arrange(plot.lists[[1]][[jj]], plot.lists[[2]][[jj]], ncol = 2)
}
}

}
\references{
Stehlik and Hermann (2015). ``Letter to the Editor''. Ann. Appl. Stat. 9
   2051. doi:10.1214/15-AOAS864 -- \url{https://projecteuclid.org/euclid.aoas/1453994190}
}
