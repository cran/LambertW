ks.test.t <-
function(y, theta=NULL){

if (is.null(theta)) {
theta=suppressWarnings(fitdistr(y,"t")$est)
fac=sqrt(theta[3]/(theta[3]-2))
theta[2]=theta[2]*fac
}

if (length(theta)==4) {
fac=sqrt(theta[4]/(theta[4]-2))
theta[3]=theta[3]*fac
theta=theta[-1]
}

fac=sqrt(theta[3]/(theta[3]-2))

s.x=(y-theta[1])/theta[2]*fac
K=suppressWarnings(ks.test(s.x, "pt", df=theta[3]))
K$method = paste("One-sample Kolmogorov-Smirnov test \n student-t with nu=", round(theta[3],2),", m=", round(theta[1],2) ,", s=", round(theta[2],2), sep="" )
K
}

