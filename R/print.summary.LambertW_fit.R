print.summary.LambertW_fit <-
function(x, ...) {
  cat("Call: ")
  print(x$call)
  cat("Estimation method: ")
  cat(x$method)
  cat("\n")
  cat("Input distribution: ")
  cat(x$distname)
  cat("\n")
  
  cat("\n Parameter estimates:\n")
  if (x$method == "IGMM") cat("WARNING: Standard Errors are only asymptotic simulation based!!! \n")
  
  printCoefmat(x$coefmat, signif.stars=TRUE)
  if (x$type == "s"){
    cat("-------------------------------------------------------------- \n")
    if (!any(x$distname == c("exp", "chi","gamma", "F"))) {
      M=rbind(x$support, x$data.range)
      colnames(M) = c("a", "b")
      rownames(M) = c("Support", "Data range")
      print(M)
      
      cat("\n p_1 = Probability that non-principal branch affects the solution: ")
      cat(x$p_1)
      cat("\n")
    }
  }
  if (x$type == "hh"){
  cat("-------------------------------------------------------------- \n")
  
  cat("\np-value for 'H_0:symmetric' versus 'H_1:skewed': ")
  cat(x$symmetry_pval)
  cat("\n")
  }

}

