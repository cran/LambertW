qLambertW <-
function(p, theta = c(0,0,1), distname=c("normal")) {

if (length(theta) == 3 & distname=="t") stop("You must specify a degrees of freedom parameter for student-t input.")

delta=theta[1]
mu_x=theta[2]
sigma_x=theta[3]
nu=theta[4]

S=support(theta)

##################################

aux=function(alpha) {
if (alpha>1) stop("Probability must be less or equal to 1.")
if (alpha<0) stop("Probability must be greater or equal to 0.")

if (alpha==0) Q=S[1]
if (alpha==1) Q=S[2]

if (alpha>0 && alpha<1) {
aux=function(y.a) {
(pLambertW(y.a, theta, distname)-alpha)^2
}

S.10=c(-10,10)*sigma_x+mu_x

intv=c(max(S.10[1], S[1]), min(S.10[2],S[2]))
fit=suppressWarnings(optimize(aux, interval=intv))
Q=fit$min
}
Q
}
round(sapply(p, aux),10)
}

