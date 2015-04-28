#' @title Common arguments for many functions
#' @name common-arguments
#' 
#' @description
#' Useful reference list with most common function arguments in this package.
#' 
#' @param y a numeric vector of real values (the observed data).
#' @param distname string; name of input distribution; see \code{\link{get_distnames}}.
#' @param type type of Lambert W \eqn{\times} F distribution: skewed \code{"s"}; 
#' heavy-tail \code{"h"}; or skewed heavy-tail \code{"hh"}.
#' @param theta list; a (possibly incomplete) list of parameters \code{alpha}, \code{beta}, \code{gamma}, 
#' \code{delta}. \code{\link{complete_theta}} fills in default values for missing entries.
#' @param beta vector; parameter \eqn{\boldsymbol \beta} of the input distribution. 
#' See \code{\link{check_beta}} on how to specify \code{beta} for each distribution.
#' @param gamma scalar; skewness parameter; default: \code{0}.
#' @param delta scalar or vector (length 2); heavy-tail parameter(s); default: \code{0}.
#' @param alpha scalar or vector (length 2); heavy tail exponent(s); default: \code{1}.
#' @param tau named vector \eqn{\tau} which defines the variable transformation. Must have at least
#' \code{'mu_x'} and \code{'sigma_x'} element; see \code{\link{complete_tau}} for details.
#' @param return.u logical; if \code{TRUE}, the function returns the calculated output that corresponds
#' to \eqn{U}, which is the zero-mean and/or unit-variance version of input \eqn{X \sim F_X}.
NULL