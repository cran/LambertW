W_2delta <-
function(z, delta = c(0,1/5)){
u = z
if (length(delta) == 1) delta = c(delta, delta)
u[z < 0] = W_delta(z[z < 0], delta = delta[1])
u[z > 0] = W_delta(z[z > 0], delta = delta[2])
invisible(u)
}

