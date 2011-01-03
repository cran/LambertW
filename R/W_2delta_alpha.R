W_2delta_alpha <-
function(z, delta = c(0,1/5), alpha = 1){
u = z
if (length(delta) == 1) delta = c(delta, delta)
if (length(alpha) == 1) alpha = c(alpha, alpha)
u[z < 0] = W_delta_alpha(z[z < 0], delta = delta[1], alpha = alpha[1])
u[z > 0] = W_delta_alpha(z[z > 0], delta = delta[2], alpha = alpha[2])
invisible(u)
}

