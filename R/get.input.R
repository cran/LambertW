`get.input` <-
function(y, theta) {
delta=theta[1]
c=theta[2]
s=theta[3]
nu=theta[4]

z=(y-c)/s
u=1/delta*W(delta*z)
x=u*s+c

if (!is.na(nu)) u=sqrt(nu/(nu-2))*u
O=NULL
O$u=u
O$x=x
return(O)
}

