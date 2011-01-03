starting_parameters <-
function(y, type = "h", distname="normal", fixed_parameters = list(alpha = 1)){

parameters.0 = NULL

location_family = FALSE
if (any(distname == c("normal", "t", "cauchy", "laplace"))) location_family = TRUE

supported_by_fitdistr = c("beta", "cauchy", "chi-squared", "exponential", "f", "gamma", "geometric", "log-normal", "lognormal", "logistic", "negative binomial", "normal", "Poisson", "t", "weibull")

if (location_family) {
	if (type == "s") theta_gmm = IGMM(y, type = "s", skewness_x = 0)$theta
	if (any(type == c("h", "hh"))) theta_gmm = IGMM(y, type = "h", kurtosis_x = 3)$theta
	}
else { # here I take the exponential distribution as a reference for a non-location_family distribution
	if (type == "s") theta_gmm = IGMM(y, type = "s", skewness_x = 2, theta.0 = c(0, mean(y), 0, 0), location_family = FALSE)$theta
	if (any(type == c("h", "hh"))) theta_gmm = IGMM(y, type = "h", kurtosis_x = 9, theta.0 = c(0, mean(y), 0, 0), location_family = FALSE)$theta
}

x_input = get.input(y, theta_gmm)

if (distname == "exp") beta.0 = suppressWarnings(fitdistr(x_input, "exponential")$est)
else if (distname == "normal") beta.0 = theta_gmm[1:2]
else if (any(distname == supported_by_fitdistr)) {
	beta.0 = suppressWarnings(fitdistr(x_input, distname)$est)
}
else if (distname == "chisq"){
 	beta.0 = mean(x_input)
}
else if (distname == "unif"){
	beta.0 = c(min(x_input), max(x_input))
}
else {
stop("Unsupported distribution. Please provide your own set of starting values for the parameters.")
}
names(beta.0) = beta_names(distname)
parameters.0$beta = beta.0
if (type == "h") {
        parameters.0$delta = theta_gmm["delta"]
        names(parameters.0$delta) = c("delta")
}
if (type == "hh") {
        parameters.0$delta = theta_gmm["delta"] + sign(skewness(y)) * c(-0.05, 0.05)
        names(parameters.0$delta) = c("delta_l", "delta_r")
}
if (type == "s") {
        parameters.0$gamma = theta_gmm["gamma"]
        names(parameters.0$gamma) = c("gamma")
}
if (is.null(fixed_parameters$alpha)) {
        if (is.null(parameters.0$alpha)) {
            parameters.0$alpha = 1
            names(parameters.0$alpha) = "alpha"
        }
}
return(parameters.0)
}

