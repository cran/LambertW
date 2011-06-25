IGMM <-
function(y, type="s",skewness_x = 0, kurtosis_x = 3, tau.0 = c(median(y), sd(y), gamma_Taylor(y), delta_Taylor(y)), robust = FALSE, tol = .Machine$double.eps^0.25, location_family = TRUE, restricted = NULL) UseMethod("IGMM")
