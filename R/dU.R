dU <-
function (u, beta = NULL, distname = c("normal")) 
{
    if (distname == "cauchy") {
        fU = function(u) dcauchy(u)
    }
    if (distname == "chisq") {
	sigma_x = sqrt(2 * beta)
        fU = function(u) dchisq(u*sigma_x, df = beta)*sigma_x
    }
    if (distname == "exp") {
        fU = function(u) dexp(u)
    }
    if (distname == "gamma") {
        fU = function(u) dgamma(u, shape = beta[1], rate = sqrt(beta[1]))
    }
    #if (distname == "laplace"){
#	fU = function(u) dlaplace(u, 0, 1/sqrt(2))
 #   }
    if (distname == "normal") {
        fU = function(u) dnorm(u)
    }
    if (distname == "t") {
        sigma_x = sqrt(beta[3]/(beta[3]-2))
        fU = function(u) dt(u*sigma_x, df = beta[3])*sigma_x
    }
    if (distname == "unif") {
        fU = function(u) dunif(u, -sqrt(12)/2, sqrt(12)/2)
    }
    return(fU(u))
}
