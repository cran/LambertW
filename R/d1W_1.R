d1W_1 <-
function(z, W_1_z = W_1(z)) {
out = rep(1, length(z))
ind.not0 = (abs(z) != 0)
out[ind.not0] = W_1_z[ind.not0]/(W_1_z[ind.not0] + 1)/z[ind.not0]
return(out)
}

