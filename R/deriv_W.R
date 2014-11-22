#' @rdname W
#' @export
deriv_W <- function(z, branch = 0, W.z = W(z, branch = branch)) {
  W.deriv <- rep(1, length(z))
  ind.not.zero <- (abs(z) != 0)
  W.deriv[ind.not.zero] <- W.z[ind.not.zero]/(W.z[ind.not.zero] + 1)/z[ind.not.zero]
  dim(W.deriv) <- dim(z)
  return(W.deriv)
} 

