#' @rdname LambertW-utils
#' @param lower.tail logical; if \code{TRUE}, probabilities are given as log(p).
#' @export
pLambertW <- function(q, distname, theta = NULL, beta = NULL, gamma = 0, delta = 0, alpha = 1, 
                      input.u = NULL, tau = NULL, log = FALSE,
                      lower.tail = FALSE) {
  
  if (is.null(theta)) {
    theta <- list(beta = beta, alpha = alpha, gamma = gamma, delta = delta)
  } 
  theta <- complete_theta(theta)
  
  if (is.null(input.u)) {
    check_distname(distname)
    check_theta(theta = theta, distname = distname)
    tau <- theta2tau(theta = theta, distname = distname)
    FU <- function(u) pU(u, beta = theta$beta, distname = distname) 
  } else {
    FU <- input.u
    if (is.null(tau)) {
      stop("You must provide a 'tau' argument if 'input.u' is not NULL.")
    }
  }
  y <- q
  
  FX <- function(x) {
    return(FU((x - tau["mu_x"])/tau["sigma_x"]))
  }
  
  type.tmp <- tau2type(tau)
  if (all(tau[grepl("delta", names(tau))] == 0) && all(tau[grepl("alpha", names(tau))] == 1) && 
        tau["gamma"] == 0) {
    cum.probs <- FX(y)
  } else {
    # begin of else
    G <- rep(NA, length(y))
    z <- (y - tau["mu_x"])/tau["sigma_x"]
    names(z) <- NULL
    ## the heavy-tail version (if theta$delta != 0)
    if (type.tmp == "h") {
      u <- W_delta_alpha(z, delta = tau["delta"], alpha = tau["alpha"])
      G <- FU(u)
    } else if (type.tmp == "hh") {
      ind.pos <- (z > 0)
      theta.l <- list(beta = theta$beta, gamma = 0, 
                      delta = tau["delta_l"], alpha = tau["alpha_l"])
      theta.r <- list(beta = theta$beta, gamma = 0, 
                      delta = tau["delta_r"], alpha = tau["alpha_r"])
      G[!ind.pos] <- pLambertW(y[!ind.pos], theta = theta.r, distname = distname)
      G[ind.pos] <- pLambertW(y[ind.pos], theta = theta.l, distname = distname)

    } else if (type.tmp == "s") {
      gamma.negative <- FALSE
      if (tau["gamma"] < 0) {
        y <- -y
        tau["gamma"] <- -tau["gamma"]
        tau["mu_x"] <- -tau["mu_x"]
        gamma.negative <- TRUE
      }
      z <- (y - tau["mu_x"])/tau["sigma_x"]
      names(z) <- NULL
      r_0 <- W_gamma(z, gamma = tau["gamma"], branch = 0)
      r_1 <- W_gamma(z, gamma = tau["gamma"], branch = -1)
      x_0 <- r_0 * tau["sigma_x"] + tau["mu_x"]
      x_1 <- r_1 * tau["sigma_x"] + tau["mu_x"]
      
      G_0 <- FX(x_0)
      G_1 <- FX(x_0) - FX(x_1)
      # G = G_0 * as.numeric(z >= 0) + G_1 * as.numeric(z < 0)
      G[z >= 0] <- G_0[z >= 0]
      G[z < 0] <- G_1[z < 0]
      G[is.na((G < -1))] <- 0
      if (gamma.negative) {
        G <- 1 - G
      }
    }
    cum.probs <- G
  }  # end of else 
  names(cum.probs) <- NULL
  
  if (lower.tail) {
    cum.probs <- 1 - cum.probs
  }
  
  if (log) {
    return(log(cum.probs))
  } else {
    return(cum.probs)
  }
} 