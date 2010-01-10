IGMM.default <-
function(y, tol=.Machine$double.eps^0.5, gamma_x = 0, theta.0=c((skewness(y)-gamma_x)/6, median(y), sd(y)), robust=FALSE){
A = rbind(0, theta.0)
colnames(A)=c("delta", "mu_x", "sigma_x")
k = 0
while (vec.norm(A[2,]-A[1,],p=2) > tol) {
k = k+1
A[1,]=A[2,]
A[2,]=delta.GMM(y, c=A[2,2], s=A[2,3], gamma_x, robust)
}

est = NULL 
est$data = y
est$theta.0 = theta.0
est$theta = A[2,]
est$iterations = k
est$hessian = diag(1/c(0.4, 1, sqrt(1/2))/sqrt(length(y)))
est$n = length(y)
est$call = match.call()
est$distname = paste("Any distribution with theoretical skewness =", gamma_x)
est$gamma_x = gamma_x
est$message = paste("Conversion reached in step", k)
est$method = c("IGMM")

class(est) = "LWest"
est
}

