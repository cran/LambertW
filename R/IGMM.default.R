IGMM.default <-
function(y, robust=FALSE, tol=10^(-6), gamma_x = 0){
A = rbind(0, c(0, mean(y), sd(y)))
colnames(A)=c("delta", "mu_x", "sigma_x")
k = 0
while (vec.norm(A[2,]-A[1,]) > tol) {
k = k+1
A[1,]=A[2,]
A[2,]=delta.GMM(y, c=A[2,2], s=A[2,3], gamma_x, robust)
}

est = NULL 
est$data = y
est$theta = A[2,]
est$distname = paste("Any distribution with theoretical skewness =", gamma_x)
est$iterations = k
est$n = length(y)
est$method = c("IGMM")
est$call = match.call()
est$gamma_x = gamma_x
est$message = paste("Conversion reached in step", k)

class(est) = "LWest"
est
}

