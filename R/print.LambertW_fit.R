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
    if (x$method == "IGMM") x$param.hat = x$theta
    print(x$param.hat)
    if (x$method == "IGMM") {
 	cat(paste("\n Obtained after", x$iterations, "iterations."))
	}
	cat("\n")
}

