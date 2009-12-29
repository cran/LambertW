support <-
function(theta) {
delta=theta[1]
mu=theta[2]
sigma=theta[3]

if (delta == 0) O=cbind(-Inf, Inf)
else {

b=1/(-delta*exp(1))*sigma+mu

if (delta >0) O=cbind(b, Inf)
if (delta <0) O=cbind(-Inf,b)
}
colnames(O) = c("a", "b")
O
}

