d1W_delta_alpha <-
function(z, delta = 1, alpha = 1){
out = rep(1, length(z))
if (all(delta == 0)) return(out)
ga = delta*alpha
W.ga = W(ga*(z^2)^alpha)

first.term = (W.ga/ga)^(1/(2*alpha)-1)
second.term = W.ga / ( ga * z *(1+W.ga))

out = sign(z) * first.term * second.term
out[is.na(out)] = 1
return(out)
}
