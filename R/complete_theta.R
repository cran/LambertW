complete_theta <-
function(theta = list(beta = c(0,1)), input = NULL){
   if (is.null(theta$alpha)) theta$alpha = 1
   if (is.null(theta$beta)) theta$beta = input$beta
   if (is.null(theta$gamma)) theta$gamma = 0
   if (is.null(theta$delta)) theta$delta = 0
   return(theta)
}
