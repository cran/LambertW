IGMM <-
function(y, skewness_x = 0, type="s", kurtosis_x = 3, theta.0 = c(median(y), sd(y), (skewness(y) - 
    skewness_x)/6, max(0, (kurtosis(y)-3)/100)), robust = FALSE, tol = .Machine$double.eps^0.5, location_family = TRUE) UseMethod("IGMM")

