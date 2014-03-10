plot.LambertW_input <-
function(x, a = NULL, b = NULL,...){

  obj = x
  tau = obj$tau
  
  left_lim_0 = FALSE
  if (any(obj$distname == c("exp","chisq", "gamma", "F"))) left_lim_0 = TRUE
  
  if (is.null(a)) a = tau[1]-4*tau[2]*(!left_lim_0) + 0.00001
  if (is.null(b)) {
    b = tau[1] + (4 + 3*(left_lim_0))*tau[2]
    if (obj$distname == "chisq") b = tau[1] + (3 + 3*(left_lim_0))*sqrt(2*obj$beta)
  }
  
  input_distname_with_beta = obj$distname_with_beta
  input_distname = obj$distname
  
  x.seq = seq(a, b, length=101)
  y.lim_d = range(c(obj$dX()(x.seq))) # y-range for density
  y.lim_p = range(c(obj$pX()(x.seq))) # y-range for cdf
  
  if (obj$distname == "chisq" && obj$beta == 1) {
  y.lim_d = c(0,min(1,y.lim_d[2]))
  }
  
  par(mfrow=c(1,2), mar=c(4,4,3,1))
  # pdf plot
  plot(obj$dX(),a,b, lwd=1, main = obj$distname_with_beta, ylab="pdf", xlab = "x", ylim = y.lim_d)
  abline(v = tau[1], lty=2)
  # cdf plot
  plot(obj$pX(),a,b, lwd=1, main = obj$distname_with_beta, ylab="cdf", xlab = "x", ylim = y.lim_p)
}
