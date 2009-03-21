`delta.01` <-
function(delta){
S=1; M=0
### for white noise output; given delta
if (delta!=0) {
S=1/(2*delta^2)*(-1 + sqrt(1+4*delta^2))
M=-delta*S
}
list(M=M,S=S)
}

