#' @title Utilities for parameter vector beta of the input distribution
#' @name beta-utils
#' 
#' @description
#' The vector \eqn{\boldsymbol \beta} parametrizes the input distribution
#' \eqn{X \sim F_X(x \mid \boldsymbol \beta)}. Depending on the distribution 
#' \eqn{\boldsymbol \beta} has different length and names: e.g., 
#' for a \code{"normal"} distribution \code{beta} is of length 
#' \eqn{2} (\code{"mu"}, \code{"sigma"}); for an \code{"exp"}onential 
#' distribution \code{beta} is a scalar (rate \code{"lambda"}).
#' 
#' @inheritParams common-arguments
#' @param beta numeric; vector \eqn{\boldsymbol \beta} of the input
#' distribution; specifications as they are for the R implementation of this
#' distribution. For example, if \code{distname = "exp"}, then \code{beta = 2}
#' means that the rate of the exponential distribution equals \eqn{2}; if
#' \code{distname = "normal"} then \code{beta = c(1,2)} means that the mean and
#' standard deviation are 1 and 2, respectively.

#' @keywords math utilities
#' 
NULL