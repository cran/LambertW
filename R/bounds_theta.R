bounds_theta <-
function(distname = "normal", beta = NULL, type = "s", fixed_theta = list(alpha = 1)){

if (any(distname == c("normal", "cauchy"))) {
	lb = c(-Inf, 0)
	ub = c(Inf, Inf)
}
else if (distname == "t") {
	lb = c(-Inf, 0, 0)
	ub = c(Inf, Inf, Inf)
}
else if (any(distname == c("exp", "chisq"))) {
	lb = c(0)
	ub = c(Inf)
}
else if (any(distname == c("unif"))) {
	lb = c(-Inf, -Inf)
	ub = c(Inf, Inf)
}
else {
lb = rep(-Inf, length(beta))
ub = rep(Inf, length(beta))
}

names(lb) = names(ub) = names(beta)
if (type == "s") {
	lb = c(lb, -Inf)
	ub = c(ub, Inf)
	names(lb)[length(lb)] = names(ub)[length(ub)] = "gamma"
}
if (type == "h") {
	lb = c(lb, 0)
	ub = c(ub, Inf)
	names(lb)[length(lb)] = names(ub)[length(ub)] = "delta"
}
if (type == "hh") {
	lb = c(lb, 0, 0)
	ub = c(ub, Inf, Inf)
	names(lb)[length(lb)-0:1] = names(ub)[length(ub)-1:0] = c("delta_l", "delta_r")
}
if (is.null(fixed_theta$alpha)) {
	lb = c(lb, 0)
	ub = c(ub, Inf)
	names(lb)[length(lb)] = names(ub)[length(ub)] = "alpha"
}

OUT = list()
OUT$lowerbound = lb
OUT$upperbound = ub
return(OUT)
}

