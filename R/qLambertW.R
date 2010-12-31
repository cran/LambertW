qLambertW <-
function(p, beta = c(0,1), gamma = 0, delta = 0, alpha = 1, distname = c("normal"), input.U = NULL){

check_LambertW_parameters(beta = beta, alpha = alpha, delta = delta, gamma = gamma, distname = distname)

theta = beta2theta(beta = beta, distname = distname)
mu_x = theta[1]
sigma_x = theta[2]

if (is.null(input.U)) q_U = function(p) qU(p, beta=beta, distname=distname)
else q_U = input.U


S = c(-Inf, Inf)
if (gamma != 0) S=support(theta)

##################################

if (gamma != 0) {

	aux=function(alpha_level) {
	if (alpha_level>1) stop("Probability must be less or equal to 1.")
	if (alpha_level<0) stop("Probability must be greater or equal to 0.")

	if (alpha_level==0) QQ=S[1]
	if (alpha_level==1) QQ=S[2]

	if (alpha_level>0 && alpha_level<1) {
		aux.p=function(y.a) {
		#(pLambertW(y.a, theta, distname)-alpha)^2
		(pLambertW(y.a, beta = beta, gamma = gamma, delta = delta, alpha = alpha, distname = distname) - alpha_level)^2
		}

		S.10=c(-10,10)*sigma_x+mu_x

		intv=c(max(S.10[1], S[1]), min(S.10[2],S[2]))
		fit=suppressWarnings(optimize(aux.p, interval=intv))
		QQ=fit$min
		} # end of 'if (alpha_level>0 && alpha_level<1)'
	QQ
	} # end of 'aux' function

}

if (gamma == 0){
	aux = function(alpha_level){
	if (alpha_level>1) stop("Probability must be less or equal to 1.")
	if (alpha_level<0) stop("Probability must be greater or equal to 0.")

	if (alpha_level==0) QQ=S[1]
	if (alpha_level==1) QQ=S[2]
        if (alpha_level > 0 && alpha_level <= 0.5){
		u_alpha = q_U(alpha_level)
		QQ = G_2delta_alpha(u_alpha, delta = delta)*sigma_x + mu_x
	}
	if (alpha_level>0.5 && alpha_level<1) {
		u_alpha = q_U(1-alpha_level)
		QQ = G_2delta_alpha(u_alpha, delta = delta)*sigma_x + mu_x
		QQ = -QQ
		}
	names(QQ) = NULL
	QQ
	}
}

return(round(sapply(p, aux),10))
}

