rU <-
function(n = NULL, beta = NULL, distname=c("normal")){
   if (distname == "cauchy"){
      uu = rcauchy(n)
   }
   if (distname == "chisq"){
      sigma_x = sqrt(2*beta)
      uu = rchisq(n, df = beta)/sigma_x
   }
   if (distname == "exp"){
      uu = rexp(n)
   }
   if (distname == "gamma"){
      uu = rgamma(n, shape = beta[1], rate = sqrt(beta[1]))
   }
   if (distname == "normal") {
      uu = rnorm(n)
   }
   if (distname =="t"){
      ss = beta2tau(beta, distname = distname)[2]
      uu = rt(n, df=beta[3])/ss
   }
   if (distname == "unif") {
      uu = runif(n, -sqrt(12)/2, sqrt(12)/2)
   }
   
   return(uu)
}
