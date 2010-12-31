Gaussianize <-
function(y, type = "h", method = "MLE") {
if (!is.matrix(y)) y = as.matrix(y)

if (method == "MLE"){
aux = function(y) {
MLE_LambertW(y, type=type, distname="normal")$theta
}
}

if (method == "IGMM"){
aux = function(y) {
IGMM(y, type=type)$theta
}
}

THETA = apply(y, 2, aux)
THETA = as.matrix(THETA, ncol = ncol(y))
X = y
for (ii in 1:ncol(THETA)) {
X[,ii] = get.input(y[,ii], theta = THETA[,ii])
}

#if (center) X = sweep(X, 2, THETA[1,], FUN = "-", check.margin = FALSE)
#if (scale) X = sweep(X, 2, THETA[2,], "/", check.margin = FALSE)

#if (method == "MLE") {
attr(X, "Gaussianized:mu") = THETA[1,]
attr(X, "Gaussianized:sigma") = THETA[2,]
#}

if (type == "h") {
attr(X, "Gaussianized:delta") = THETA["delta",]
}
if (type == "s"){
attr(X, "Gaussianized:gamma") = THETA["gamma",]
}

if (type == "hh"){
attr(X, "Gaussianized:delta_l") = THETA["delta_l",]
attr(X, "Gaussianized:delta_r") = THETA["delta_r",]
}
X
}

