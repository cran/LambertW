IGMM <-
function(y, skewness_x = 0, type="s", kurtosis_x = 3, tau.0 = c(median(y), sd(y), (skewness(y) - 
    skewness_x)/6, delta_Taylor(y)), robust = FALSE, tol = .Machine$double.eps^0.25, location_family = TRUE) UseMethod("IGMM")

