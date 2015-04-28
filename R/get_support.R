#' @title Computes the truncated support for skewed Lambert W distributions
#' 
#' @description
#' If the input \eqn{X \sim F} has support on the entire real line
#' \eqn{(-\infty, \infty)}, then the skewed Lambert W \eqn{\times} F
#' distribution has truncated support \eqn{[a,b]}, \eqn{a,b \in R \cup \pm
#' \infty} depending on \eqn{\boldsymbol \beta} and (the sign of) \eqn{\gamma}.
#' 
#' @name get_support
#' @inheritParams common-arguments
#' @inheritParams loglik_penalty
#' @return 
#' A vector of length 2 with names \code{'lower'} and \code{'upper'}.
#' @details
#' Half-open interval on the real line (if \eqn{\gamma \neq 0}) for
#' input with support on the entire real line. For \eqn{\gamma = 0} the support
#' of Y is the same as for X. Heavy-tail Lambert W RVs are not affected by
#' truncated support (since \eqn{\delta \geq 0} by definition); thus
#' support is \code{c(lower = -Inf, upper = Inf)}.
#' @keywords math
#' @export
#' @examples
#' 
#' get_support(c(mu_x = 0, sigma_x = 1, gamma = 0)) # as gamma = 0
#' # truncated on the left since gamma > 0
#' get_support(c(mu_x = 0, sigma_x = 1, gamma = 0.1)) 
#' 
#' # no truncation for heavy tail(s)
#' get_support(c(mu_x = 0, sigma_x = 1, delta = 0.1))

get_support <- function(tau, is.non.negative = FALSE) {
  stopifnot(is.logical(is.non.negative))
  tau <- complete_tau(tau)
  check_tau(tau)
  if (is.na(tau["gamma"])) {
    tau["gamma"] <- 0
  } 

  if (is.non.negative) {
    # assumes that gamma, delta, alpha >= 0
    rv.support <- c(0, Inf)
  } else {
    if (tau["gamma"] == 0) {
      rv.support <- c(-Inf, Inf) 
    } else {
      bb <- 1/(-tau["gamma"] * exp(1)) * tau["sigma_x"] + tau["mu_x"]
      if (tau["gamma"] > 0) {
        rv.support <- c(bb, Inf)
      } else if (tau["gamma"] < 0) {
        rv.support <- c(-Inf, bb)
      }
    }
  }
  names(rv.support) <- c("lower", "upper")
  return(rv.support)
}
