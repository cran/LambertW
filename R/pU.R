pU <-
function (u, beta = NULL, distname = c("normal")) 
{
    if (distname == "cauchy"){
	FU = function(u) pcauchy(u)
	}
    if (distname == "chisq") {
	ss = sqrt(2 * beta)
        FU = function(u) pchisq(u*ss, df = beta)
    }
    if (distname == "exp") {
        FU = function(u) pexp(u)
    }
    if (distname == "normal") {
        FU = function(u) pnorm(u)
    }
    if (distname == "t") {
	ss = beta2tau(beta, distname=distname)[2]
        FU = function(u) pt(u*ss, df = beta[3])
    }
    if (distname == "unif") {
        FU = function(u) punif(u, -sqrt(12)/2, sqrt(12)/2)
    }

    return(FU(u))
}

