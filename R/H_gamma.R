H_gamma <-
function(u, gamma = 0) {
if (gamma == 0) return(u)
1/gamma*H(u*gamma)
}
