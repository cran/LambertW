W_delta_alpha <-
function(z, delta = 0, alpha = 1){
if (delta == 0) return(z)
u = sign(z)*(W(alpha * delta*z^2)/(alpha*delta))^(1/(2*alpha))
return(u)
}

