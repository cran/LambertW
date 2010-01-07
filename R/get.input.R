get.input <-
function(y, theta, return.u=FALSE) {
delta=theta[1]
c=theta[2]
s=theta[3]
nu=theta[4]

z=(y-c)/s
u=W_delta(z, delta)
x=u*s+c

if (return.u)  {
if (!is.na(nu)) u=sqrt(nu/(nu-2))*u
O=NULL
O$u=u
O$x=x
return(O)
}
else return(x)
}

