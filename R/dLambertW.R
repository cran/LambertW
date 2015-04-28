#' @rdname LambertW-utils
#' @export
dLambertW <- function(y, distname = NULL, theta = NULL, beta = NULL, gamma = 0, delta = 0, 
                      alpha = 1, input.u = NULL, tau = NULL, log = FALSE) {
  
  if (is.null(theta)) {
    theta <- list(beta = beta, alpha = alpha, gamma = gamma, delta = delta)
  } 
  theta <- complete_theta(theta)
  
  if (is.null(input.u)) {
    check_distname(distname)
    check_theta(theta = theta, distname = distname)
    tau <- theta2tau(theta = theta, distname = distname)
    fU <- function(u) dU(u, beta = theta$beta, distname = distname) 
  } else {
    fU <- input.u
    if (is.null(tau)) {
      stop("You must provide a 'tau' argument if 'input.u' is not NULL.")
    }
  }
  
  fX <- function(x) fU((x - tau["mu_x"])/tau["sigma_x"])/tau["sigma_x"]
  
  type.tmp <- tau2type(tau)
  if (all(tau[grepl("delta", names(tau))] == 0) && all(tau[grepl("alpha", names(tau))] == 1) && 
        tau["gamma"] == 0) {
    gg <- fX(y)
  } else {
    gg <- rep(NA, length(y))
    zz <- (y - tau["mu_x"])/tau["sigma_x"]
    names(zz) <- NULL
    ## the heavy-tail version (if delta != 0)
    if (type.tmp == "h") {
      uu <- W_delta_alpha(zz, delta = tau["delta"], alpha = tau["alpha"])
      # g = fU(u) * deriv_W_delta(z, delta=delta) * tau["sigma_x"] g = 1/tau["sigma_x"] * sign(z) *
      # fU(sign(z) * u) * u / (z*(1 + delta*u^2)) #deriv_W_delta(z, delta=delta) *
      # tau["sigma_x"]
      gg <- 1/tau["sigma_x"] * fU(uu) * deriv_W_delta_alpha(zz, delta = tau["delta"], 
                                                            alpha = tau["alpha"])
    } else if (type.tmp == "hh") {
      ind.pos <- (zz > 0)
      theta.l <- list(beta = theta$beta, gamma = 0, 
                      delta = tau["delta_l"], alpha = tau["alpha_l"])
      theta.r <- list(beta = theta$beta, gamma = 0, 
                      delta = tau["delta_r"], alpha = tau["alpha_r"])
      gg[!ind.pos] <- dLambertW(y[!ind.pos], theta = theta.l, distname = distname)
      gg[ind.pos] <- dLambertW(y[ind.pos], theta = theta.r, distname = distname)
    } else if (type.tmp == "s") {
      if (tau["gamma"] < 0) {
        # revert data and signs of gamma and mu_x so that we only have to implement the gamma > 0 case
        y <- -y
        tau["gamma"] <- -tau["gamma"]
        tau["mu_x"] <- -tau["mu_x"]
      }
      zz <- (y - tau["mu_x"])/tau["sigma_x"]
      names(zz) <- NULL
      r_0 <- W_gamma(zz, gamma = tau["gamma"], branch = 0)
      r_1 <- W_gamma(zz, gamma = tau["gamma"], branch = -1)
      x_0 <- r_0 * tau["sigma_x"] + tau["mu_x"]
      x_1 <- r_1 * tau["sigma_x"] + tau["mu_x"]
      g_0 <- fX(x_0) * deriv_W(tau["gamma"] * zz)
      g_1 <- fX(x_0) * deriv_W(tau["gamma"] * zz) - fX(x_1) * deriv_W(tau["gamma"] * zz, branch = -1)
      gg <- g_0 * as.numeric(zz >= 0) + g_1 * as.numeric(zz < 0)
      gg[is.na(gg < -1)] <- 0
    }
  } # end of else 
  names(gg) <- NULL
  if (log) {
    return(log(gg))
  } else {
    return(gg)
  } 
}
