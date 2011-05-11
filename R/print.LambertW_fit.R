print.LambertW_fit <-
function(x, ...) {
    cat("Call: ")
    print(x$call)
    cat("Estimation method: ")
    cat(x$method)
    cat("\n")
    cat("Input distribution: ")
    cat(x$distname)
    cat("\n")
    cat("Lambert W type ('h' same tails; 'hh' different tails; 's' skewed): ")
    cat(x$type)
    cat("\n")
    cat("\n Parameter estimates:\n")
    if (x$method == "IGMM") x$param.hat = x$tau
    print(x$param.hat)
    if (x$method == "IGMM") {
	  one_param = "gamma"
        if (x$type == "h") one_param = "delta"
 	 cat(paste("\n Obtained after", x$iterations, "iterations for mu_x and sigma_x, and \n on average",round(x$sub_iterations/x$iterations,2),"iterations to find the optimal", one_param,"in each run."))
	  }
	  cat("\n")
}

