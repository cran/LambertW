d1W_delta <-
function(z, delta = 1){
out = rep(1, length(z))
if (delta == 0) return(out)
ind.0 = (z == 0)
value = 1
value[!ind.0] = W(delta * z[!ind.0]^2)
out[!ind.0] = delta^(-0.5) * value[!ind.0]^(-0.5) * value[!ind.0] / (z[!ind.0]*(1+value[!ind.0]))
out[!ind.0] = sign(z[!ind.0]) * out[!ind.0]
return(out)
}
