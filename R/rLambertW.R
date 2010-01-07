rLambertW <-
function(n, theta=c(0,0,1), distname=c("normal"), input=NULL, ZmUv=FALSE, return.input=FALSE) {

if (length(theta) == 3 & distname=="t") stop("You must specify a degrees of freedom parameter for student-t input.")

delta=theta[1]

####### Zero mean, Unit variance output ?!
if (ZmUv!=FALSE) { 
if (distname=="t") stop("Zero-mean, unit-variance input only for Gaussian input. Provide a theta for Gaussian input.")

theta = delta.01(delta)
}

mu_x=theta[2]
sigma_x=theta[3]
nu=theta[4]

### default is Gaussian X
if (distname=="normal") x=rnorm(n, mean=mu_x, sd=sigma_x)
if (distname=="t") {
fac=sqrt(nu/(nu-2))
x=rt(n, df=nu)/fac*sigma_x+mu_x
}

if (!is.null(input)) {
x=input
mu_x=mean(x)
sigma_x=sd(x)
}

u=(x-mu_x)/sigma_x
z=u*exp(delta*u)
y=z*sigma_x+mu_x
if (return.input) y=list(x=x,y=y)
y
}

