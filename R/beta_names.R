beta_names <-
function(distname = c("normal")){
parameter.names = c("NA")
if (distname == "cauchy") parameter.names = c("location", "scale")
if (distname == "chisq") parameter.names = c("df")
if (distname == "gamma") parameter.names = c("shape", "rate")
if (distname == "exp") parameter.names =c("rate lambda")
if (distname == "F") parameter.names =c("df1","df2")
if (distname == "laplace") parameter.names = c("location", "scale")
if (distname == "normal") parameter.names = c("mu", "sigma")
if (distname == "t") parameter.names =c("ncp", "s", "df")
if (distname == "unif") parameter.names = c("min", "max")

return(parameter.names)
}

