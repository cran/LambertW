dLambertW <-
function(y, theta=c(0,0,1), distname=c("normal")) {
delta=theta[1]
mu_x=theta[2]
sigma_x=theta[3]
nu=theta[4]

if (length(theta) == 3 & distname=="t") stop("You must specify a degrees of freedom parameter for student-t input.")

if (delta < 0) {
y=-y
delta=-delta
mu_x=-mu_x
}

if (distname=="t") {
if (nu <= 2.1) {
nu=2.1
#print("Infinite Variance - Normalization impossible. Degrees of freedom have been changed to nu=2.5")
}
}

############################# Transform variables
z=(y-mu_x)/sigma_x
r_0=W_delta(z, delta=delta)
r_1=W_delta_1(z, delta=delta)

x_0=r_0*sigma_x+mu_x
x_1=r_1*sigma_x+mu_x

############################# Specify the input densities f_X and the implicated p.value
######################## Gaussian
f_x=function(X) dnorm(X, mean=mu_x, sd=sigma_x)
######################## t-student
if (distname=="t") {
fac=sqrt(nu/(nu-2))
s=sigma_x/fac
f_x=function(X) dt((X-mu_x)/s, df=nu)/s
}

if (delta == 0) g=f_x(y)
else{
g_0=f_x(x_0)*d1W(delta*z)
g_1=f_x(x_0)*d1W(delta*z)-f_x(x_1)*d1W_1(delta*z)

g=g_0*as.numeric(z>=0)+g_1*as.numeric(z<0)
g[is.na((g < -1))]=0 ## a trick to determine the NaN values of g
}
g
}

