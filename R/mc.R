mc <-
function(x) {

if (length(x)> 3000) stop("\n Observation has more than 3000 observations. Please reduce number")

#### kernel
h=function(z1,z2) {
(z2+z1)/(z2-z1)
}

z=sort(x, decreasing=TRUE)
z=z-median(z)

Z.neg=z[z<0]
Z.pos=z[z>0]

B=outer(Z.neg, Z.pos, h)
median(B)
}
