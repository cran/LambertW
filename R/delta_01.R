delta_01 <-
function(delta, mu_y=0, sigma_y=1){
moments = function(n=1, delta = 0){
if (n %% 2) return(0)
else factorial(n) * (1-n*delta)^(-(n+1)/2)/ (2^(n/2) * factorial(n/2))
}

### for white noise WN(0,1) given delta

c(0, sqrt(1/(moments(n=2, delta))),0,delta)
}

