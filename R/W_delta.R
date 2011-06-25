W_delta <-
function(z, delta = 0){
if (delta == 0) return(z)
u = sign(z)*(W(delta*z^2)/(delta))^(1/(2))
return(u)
}
