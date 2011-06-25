starting_theta <-
function(y, type = "h", distname="normal", fixed_theta = list(alpha = 1)){

theta.0 = NULL

location_family = FALSE
if (any(distname == c("normal", "t", "cauchy", "laplace"))) location_family = TRUE

supported_by_fitdistr = c("beta", "cauchy", "chi-squared", "exponential", "f", "gamma", "geometric", "log-normal", "lognormal", "logistic", "negative binomial", "normal", "Poisson", "t", "weibull")

if (location_family) {
	if (type == "s") tau_gmm = IGMM(y, type = "s", skewness_x = 0)$tau
	if (any(type == c("h", "hh"))) tau_gmm = IGMM(y, type = "h", kurtosis_x = 3, restricted = TRUE)$tau
	}
else { # here I take the exponential distribution as a reference for a non-location_family distribution
	if (type == "s") tau_gmm = IGMM(y, type = "s", skewness_x = 2, tau.0 = c(0, mean(y), 0, 0), location_family = FALSE, restricted = TRUE)$tau
	if (any(type == c("h", "hh"))) tau_gmm = IGMM(y, type = "h", kurtosis_x = 9, tau.0 = c(0, mean(y), 0, 0), location_family = FALSE, restricted = TRUE)$tau
}

x_input = get.input(y, tau_gmm)

if (distname == "exp") beta.0 = suppressWarnings(fitdistr(x_input, "exponential")$est)
else if (distname == "normal") beta.0 = tau_gmm[1:2]
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
theta.0$beta = beta.0
if (type == "h") {
        theta.0$delta = tau_gmm["delta"]
        names(theta.0$delta) = c("delta")
}
if (type == "hh") {
        theta.0$delta = tau_gmm["delta"] + sign(skewness(y)) * c(-0.05, 0.05)
        names(theta.0$delta) = c("delta_l", "delta_r")
}
if (type == "s") {
        theta.0$gamma = tau_gmm["gamma"]
        names(theta.0$gamma) = c("gamma")
}
if (is.null(fixed_theta$alpha)) {
        if (is.null(theta.0$alpha)) {
            theta.0$alpha = 1
            names(theta.0$alpha) = "alpha"
        }
}
return(theta.0)
}
