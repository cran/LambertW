#' @rdname distname-utils
#' 
#' @description
#' \code{get_distname_family} determines whether a distribution is a location, scale, or 
#' location-scale family.
#' 
#' @return 
#' \code{get_distname_family} returns a list with
#' \item{location}{ logical; if \code{TRUE}, the distribution is a location family,}
#' \item{scale}{ logical; if \code{TRUE}, the distribution is a scale family.}
#' 
#' @export
#' @examples
#' 
#' get_distname_family("normal")


get_distname_family <- function(distname) {
  
  check_distname(distname)
  out <- list(location = FALSE,
              scale = FALSE)
  if (distname %in% c("cauchy", "normal", "t", "unif")) {
    out$location <- TRUE
    out$scale <- TRUE
  } else if (distname %in% c("exp", "chisq", "gamma", "F", "f")) {
    out$scale <- TRUE
  }
  return(out)
}