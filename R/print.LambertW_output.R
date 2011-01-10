print.LambertW_output <-
function(x, ...){
    #cat("Call: ")
    #print(x$call)
    #cat("Type of Transformation: ")
    #cat(x$type)
    #cat("\n")

if (is.null(x$theta$gamma)) x$theta$gamma = 0
if (is.null(x$theta$delta)) x$theta$delta = 0
if (is.null(x$theta$alpha)) x$theta$alpha = 1
    cat(" Input distribution: ")
distname_input = substr(x$distname, 13, nchar(x$distname))
    cat(distname_input)
    cat("\n")
    #cat("Lambert W type ('h' same tails; 'hh' different tails; 's' skewed): ") 
    cat(" Output distribution: ")
pre_text = NULL
if (x$type == "s") pre_text = "skewed"
if (x$type == "h") pre_text = "heavy-tail (one parameter)"
if (x$type =="hh") pre_text = "heavy-tail (two parameters)" 
    cat(paste("A", pre_text, x$distname_with_beta))
    #cat("\n")
    cat("\n with parameters:\n")
    cat(paste(paste(" ", beta_names(distname_input)," = ", sep=""), round(x$theta$beta,3), sep=""))
    cat("\n")
    if (x$type == "s") cat(paste(" gamma =", round(x$theta$gamma,3)))
    if (x$type == "h") cat(" delta =", round(x$theta$delta,3))
    if (x$type == "hh") cat(paste(" ",paste(c("delta_l"," delta_r"),"=", sep=""), round(x$theta$delta,3), sep="   "))
    cat("\n")
    if (x$theta$alpha != 1){
    cat(paste(" alpha =", round(x$theta$alpha,3)))
    cat("\n")
    }
}

