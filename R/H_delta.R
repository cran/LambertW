`H_delta` <-
function(u, delta) {
if (delta ==0) return(u)
1/delta*H(u*delta)
}

