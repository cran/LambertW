d1W <-
function(z, W_z = W(z)) {
out = rep(1, length(z))
ind.not0 = (abs(z) != 0)
out[ind.not0] = W_z[ind.not0]/(W_z[ind.not0] + 1)/z[ind.not0]
return(out)
}

