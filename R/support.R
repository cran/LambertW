support <-
function(theta, gamma = NULL) {
mu=theta[1]
sigma=theta[2]
if (is.null(gamma)) gamma=theta[3]

if (gamma == 0) SUP=cbind(-Inf, Inf)
else {

b=1/(-gamma*exp(1))*sigma+mu

if (gamma >0) SUP = cbind(b, Inf)
if (gamma <0) SUP = cbind(-Inf,b)
}
colnames(SUP) = c("a", "b")
rownames(SUP) = NULL
SUP
}
