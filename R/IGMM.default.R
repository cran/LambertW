`IGMM.default` <-
function(y, robust=FALSE, tol=10^(-6), gamma_x = 0, ...){

num.iter=20
N=length(y)
A=matrix(ncol=3, nrow=num.iter,0)
colnames(A)=c("delta", "mu_x", "sigma_x")
A[1,]=c(0, mean(y), sqrt(var(y)))

for (i in 1:num.iter) {
out=delta.GMM(y, c=A[i,2], s=A[i,3], gamma_x)
A[i+1,]=out
B=A[i+1,]-A[i,]
v.n=vec.norm(B)

if (v.n < tol) break # stop if converges
}
theta.e=A[i+1,]

est = NULL 
est$theta.0 = A[2,]
est$theta = theta.e
est$distname = paste("Any distribution with theoretical skewness equal", gamma_x)
est$iterations = i
est$method = c("IGMM")
est$call = match.call()
est$gamma_x = gamma_x

B=diag(c(0.4^2, 1, 1/2))/sqrt(length(y))*0.4

est$hessian = -solve(B)
est$call = match.call()
est$message = paste("Conversion reached in step", i)
est$data = y

class(est) = "LWest"
est
}

