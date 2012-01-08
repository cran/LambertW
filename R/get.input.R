get.input <-
function (y, tau, return.u = FALSE) {
  if (class(y) == "LambertW_fit") {
    tau = y$tau
    y = y$data
  }
  if (is.na(tau["alpha"])) {
    tau = c(tau, 1)
    names(tau)[length(tau)] = "alpha"
  }
  if (is.na(tau["gamma"])) {
    tau = c(tau[1:2], 0, tau[-c(1:2)])
    names(tau)[3] = "gamma"
  }
  if (is.na(tau["delta"]) && is.na(tau["delta_l"])) {
    tau = c(tau[1:3], 0, tau[-c(1:3)])
    names(tau)[4] = "delta"
  }
  cc = tau[1]
  ss = tau[2]
  gamma = tau["gamma"]
  delta = tau[-c(1:3)][-length(tau[-c(1:3)])]
  alpha = tau["alpha"]
  zz = (y - cc)/ss
  if (gamma != 0 & all(delta == 0)) {
    uu = W_gamma(zz, gamma)
  } 
  if (any(delta != 0) & gamma == 0) {
    uu = W_2delta_alpha(zz, delta = delta, alpha = alpha)
  } 
  if (all(delta == 0) && gamma == 0) {
      uu = zz
  }
  xx = uu * ss + cc
  if (return.u) {
    nu = tau[4]
    if (!is.na(nu)) uu = sqrt(nu/(nu - 2)) * uu
    out = list()
    out$u = uu
    out$x = xx
    return(out)
  }
  else return(xx)
}
