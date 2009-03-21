`pLambertW` <-
function(y, theta=c(0,0,1), distname="normal") {
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

############################# Specify the input cdf F_X and the implicated p.value
F_x=function(X) pnorm(X, mean=mu_x, sd=sigma_x)
if (distname=="t") {
fac=sqrt(nu/(nu-2))
F_x=function(X) pt((X-mu_x)/fac*sigma_x, df=nu)
}

G=0
G_0=F_x(x_0)
G_1=F_x(x_0)-F_x(x_1)

G=G_0*as.numeric(z>=0)+G_1*as.numeric(z<0)
#if (is.na(G)) G=0

if (delta ==0) {G=F_x(y)}

G
}

