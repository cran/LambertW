complete_LambertW_parameters <-
function(parameters = list(beta = c(0,1)), input = NULL){
if (is.null(parameters$alpha)) parameters$alpha = 1
if (is.null(parameters$beta)) parameters$beta = input$beta
if (is.null(parameters$gamma)) parameters$gamma = 0
if (is.null(parameters$delta)) parameters$delta = 0
return(parameters)
}

