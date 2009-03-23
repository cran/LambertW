`delta.test` <-
function(object, ...){
DNAME = deparse(substitute(object$data))
n=length(object$data)

A=sqrt(n)*object$theta[1]/0.4
names(A)=NULL
pval=1-pnorm(A)
RVAL <- list(statistic = c(Statistic = A), p.value = pval, method = "Simulation based GMM significanc test (Asymptotic Gaussian)", 
        data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}

