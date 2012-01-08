W_gamma <-
function(z, gamma = 0) {
if (gamma == 0) return(z) 
W(gamma*z)/gamma
}
