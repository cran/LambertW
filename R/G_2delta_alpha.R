G_2delta_alpha <-
function(u, delta=c(0,1/5), alpha = c(1,1)){
z = u
z[u < 0] = G_delta_alpha(u[u <0], delta=delta[1], alpha = alpha[1])
z[u > 0] = G_delta_alpha(u[u >0], delta=delta[2], alpha = alpha[2])
invisible(z)
}

