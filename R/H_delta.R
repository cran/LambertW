H_delta <-
function(u, delta = 0) {
if (delta ==0) return(u)
1/delta*H(u*delta)
}

