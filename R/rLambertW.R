`rLambertW` <-
function(n, theta=c(0,0,1), distname="normal", innov=NULL, ZmUv=FALSE) {

if (length(theta) == 3 & distname=="t") stop("You must specify a degrees of freedom parameter for student-t input.")

### default is Gaussian X
delta=theta[1]
mu_x=theta[2]
sigma_x=theta[3]
nu=theta[4]

if (ZmUv!=FALSE) { ####### Zero mean, Unit variance output ?!
mu_x=delta.01(delta)$M
sigma_x=delta.01(delta)$S
}

if (distname=="normal") x=rnorm(n, mean=mu_x, sd=sigma_x)
if (distname=="t") {
fac=sqrt(nu/(nu-2))
x=rt(n, df=nu)/fac*sigma_x+mu_x
}

if (!is.null(innov)) {
x=innov
mu_x=mean(x)
sigma_x=sd(x)
}

u=(x-mu_x)/sigma_x
z=u*exp(delta*u)
y=z*sigma_x+mu_x
y
}

