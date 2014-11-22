#' @rdname beta-utils
#' @description
#' \code{get_beta_names} returns (typical) names for \eqn{\boldsymbol \beta}
#' and the given distribution. E.g., for a \code{'normal'} distribution the names of the
#' two-dimenstional \code{beta} are \code{'mu'} and \code{'sigma'}.
#' @return
#' \code{get_beta_names} returns a vector of strings for each element of \code{beta}.
#' @export
#' @examples
#' get_beta_names("normal")
#' get_beta_names("exp")
#' 
get_beta_names <- function(distname) {
  check_distname(distname)
  
  switch(distname,
         cauchy = {beta.names <- c("location", "scale")}, # cauchy does not have finite mean/variance          
         chisq = {beta.names <- "df"},
         exp = {beta.names <- c("lambda")},
         f = {beta.names <- c("df1", "df2")},
         gamma = {beta.names <- c("shape", "scale")},
         laplace = {beta.names <- c("location", "scale")},
         normal = {beta.names <- c("mu", "sigma")},
         t = {beta.names <- c("location", "scale", "df")},
         unif = {beta.names <- c("min", "max")},
         user = {beta.names <- NULL})  # user defined distributions return NULL
  return(beta.names)
} 
