Gaussianize <-
function(y, type = "h", method = "MLE") {
if (!is.matrix(y)) y = as.matrix(y)

if (method == "MLE"){
aux = function(y) {
MLE_LambertW(y, type=type, distname="normal")$tau
}
}

if (method == "IGMM"){
aux = function(y) {
IGMM(y, type=type)$tau
}
}

TAU = apply(y, 2, aux)
TAU = as.matrix(TAU, ncol = ncol(y))
X = y
for (ii in 1:ncol(TAU)) {
X[,ii] = get.input(y[,ii], tau = TAU[,ii])
}

#if (center) X = sweep(X, 2, TAU[1,], FUN = "-", check.margin = FALSE)
#if (scale) X = sweep(X, 2, TAU[2,], "/", check.margin = FALSE)

#if (method == "MLE") {
attr(X, "Gaussianized:mu") = TAU[1,]
attr(X, "Gaussianized:sigma") = TAU[2,]
#}

if (type == "s"){
attr(X, "Gaussianized:gamma") = TAU["gamma",]
}

if (type == "h") {
attr(X, "Gaussianized:delta") = TAU["delta",]
}

if (type == "hh"){
attr(X, "Gaussianized:delta_l") = TAU["delta_l",]
attr(X, "Gaussianized:delta_r") = TAU["delta_r",]
}
X
}

