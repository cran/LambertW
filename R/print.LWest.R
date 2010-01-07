print.LWest <-
function(x, ...) {
    cat("Call: ")
    print(x$call)
    cat("Estimation method: ")
    cat(x$method)
    cat("\n")
    cat("Input distribution: ")
    cat(x$distname)
    cat("\n")
    cat("\n Parameter estimate:\n")
    print(x$theta)
    cat("\n")
}

