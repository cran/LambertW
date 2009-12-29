d1W_1 <-
function(z) {
if (abs(z[1])== 0) out=1
else out=(exp(W_1(z))+z)^(-1)
return(out)
}

