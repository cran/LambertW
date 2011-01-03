qU <-
function (p, beta = NULL, distname = c("normal")) 
{
    if (distname == "cauchy"){
    	qU = function(p) qcauchy(p)
	}
    if (distname == "chisq") {
	ss = sqrt(2 * beta)
        qU = function(p) qchisq(p, df = beta)/ss
    }
    if (distname == "exp") {
        qU = function(p) qexp(p)
    }
    if (distname == "normal") {
        qU = function(p) qnorm(p)
    }
    if (distname == "t") {
        ss = beta2theta(beta, distname=distname)[2]
        qU = function(p) qt(p, df = beta[3])/ss
    }
    if (distname == "unif") {
        qU = function(p) qunif(p, -sqrt(12)/2, sqrt(12)/2)
    }

    return(qU(p))
}

