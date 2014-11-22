#' @rdname U-utils
#' @export
pU <- function(u, beta, distname) {
  
  check_distname(distname)
  names(beta) <- get_beta_names(distname)
  check_beta(beta, distname)
  sigma.x <- beta2tau(beta, distname)["sigma_x"]
  switch(distname,
         cauchy = {
           FU <- function(u) pcauchy(u)
         },
         chisq = {
           FU <- function(u) pchisq(u * sigma.x, df = beta)
         },
         exp = {
           FU <- function(u) pexp(u)
         },
         "f" = {
           FU <- function(u) pf(u * sigma.x, beta[1], beta[2])
         },
         gamma = {
           FU <- function(u) pgamma(u * sigma.x, shape = beta["shape"], scale = beta["scale"])
         },
         laplace = {
           #TODO
         },
         normal = {
           FU <- function(u) pnorm(u)
         },
         t = {
           ss <- sigma.x / beta["scale"]
           FU <- function(u) pt(u * ss, df = beta["df"])
         }, 
         unif = {
           FU <- function(u) punif(u, -sqrt(3), sqrt(3))
         }
        )
 
  return(FU(u))
} 