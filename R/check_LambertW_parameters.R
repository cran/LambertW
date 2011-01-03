check_LambertW_parameters <-
function(alpha = 1, beta = NULL, gamma = 0, delta=0, distname = NULL){
if (any(gamma != 0) && any(delta != 0)) warning("Both parameters are set to non-zero values. \n  Either gamma (skewness) or delta (heavy-tails) must be set to 0.")
if (any(gamma != 0) && any(alpha != 1)) warning("Alpha == 1 for skewed Lambert W distributions (gamma !=0). If gamma != 0, then alpha = 1 (default value).")
if ( distname =="normal" && (length(beta) != 2 || beta[2] < 0)) warning("beta for a Normal distribution must be a vector of length 2, where the second entry is positive.")
if (distname == "t" && (length(beta) != 3 || beta[2] <0 || beta[3] <0) ) warning("You must specify a degrees of freedom parameter for student-t input. \n  Check if the scale or the degree of freedom parameter is less than 0." )
if (distname == "exp" && (length(beta) != 1 || beta[1] <= 0)) warning("beta for an exponential distribution must be a one number and positive.")
}

