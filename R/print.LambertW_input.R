print.LambertW_input <-
function(x, ...){
    cat(" Input distribution: ")
    cat(x$distname)
    cat("\n with parameters:")
    cat(paste(paste(" ", beta_names(x$distname)," = ", sep=""), round(x$beta,3), sep=""))
    cat("\n")
}
