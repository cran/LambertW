theta2params <-
function(theta){
   params = NULL
   for (ii in 1:length(theta)) {
        params = c(params, theta[[ii]])
    }
   #params = unlist(theta)
   #names(params)[1:length(theta$beta)] = beta_names(distname = distname)
   return(params)
}

