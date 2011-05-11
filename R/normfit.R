normfit <-
function(data, volatility=FALSE, plot.it=TRUE, pch = 1, legend = TRUE) {
  # various normality tests (both graphical and statistical)
  # Input: Either a data-vector or a model (will use the residuals of the model)
  # Testing vector 'x': normfit(x)
  # Testing residuals of model 'mod': normfit(mod) or directely as normfit(mod$res)
  if (!is.numeric(data)) data=data$res
  
  data.label = deparse(substitute(data))
  
  if (plot.it) {
    T=length(data)
    h=hist(data, plot=FALSE)
    mu=mean(data)
    sigma=sd(data)
    range.data=c(min(data)-0.1*abs(min(data)),max(data)+0.1*abs(max(data)))
    x=seq(range.data[1], range.data[2], length=(2*T))
    y=dnorm(x,mu,sigma)
    
    title=""
    if (volatility) {
      mfrow=c(3,2)
      data = ts(data)
    }
    else mfrow=c(2,2)
    par(mfrow=mfrow, mar=c(4.5,4,1.5,1))
    
    
    plot(data, pch = pch, ylab = data.label)
    
    #########
    hist_dens=function(y) {
  
      aux.compare=function(X) {
      dnorm(X, mean=mean(y), sd=sd(y))
      }
      
      x_l=range(y)[1]-0.25*abs(range(y)[1])
      x_u=range(y)[2]+0.25*abs(range(y)[2])
      
      COL=c(1,2)
      LWD=c(2,2)
      LTY=c(1,2)
      
      n=length(y)
      BS = ceiling((range(y)[2]-range(y)[1])/(3.96 * sd(y)*n^(-1/3)))
      
      H=hist(x=y, breaks=BS, plot=FALSE)
      D.np=density(y)$y
      D.p=aux.compare(seq(x_l, x_u, length=100))
      
      hist(y, BS, xlim=c(x_l, x_u), ylim=range(D.np, D.p, H$intensities)*1.1, prob=TRUE, density=10, col="darkgray", main="Density estimates", ylab="", xlab = data.label)
      lines(density(y), lwd=LWD[1], main=paste("Density Estimates"))
      plot(aux.compare, x_l,x_u,  add=TRUE, lty=LTY[2], col=COL[2], lwd=LWD[2])
      
      
      if (legend){
      
      if (skewness(y) >= 0) {
      legend.pos = "right"
      }
      if (skewness(y) < 0) {
      legend.pos = "left" 
      }
      
      
      legend(paste("top", legend.pos, sep=""), c(paste("mean:", round(mean(y), 2)),paste("sd:   ", round(sd(y), 2)),paste("skew:", round(skewness(y), 2)), paste("kurt:  ", round(kurtosis(y), 2))) )
      legend(legend.pos, c("Kernel","Gaussian"), lty=LTY, col=COL, lwd=LWD)
      #legend(legend.pos, c("Kernel",paste("N(", round(mean(y), 2),",",round(var(y), 2) ,")", sep="")), lty=LTY, col=COL, lwd=LWD)
      }
    }
    ########
    acf(data)
    hist_dens(data)
    qqnorm(data)
    qqline(data)
    
    if (volatility) {
    plot(data**2)
    acf(data**2)
    }
    par(mfrow=c(1,1))
  }
  
  if (length(data) < 5001){
    SW=shapiro.test(data)
    SF=sf.test(data)
  }
  else {
    print("Shapiro-Wilk and Shapiro-Francia are not available for sample size > 5000.")
    SW = NA
    SF = NA
  }
  
  AD=ad.test(data)
  list(sw=SW, sf=SF,ad=AD)
}

