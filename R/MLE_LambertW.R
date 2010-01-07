MLE_LambertW <-
function(y, distname=c("normal"), theta.0=IGMM(y)$theta, hessian=TRUE) UseMethod("MLE_LambertW")

