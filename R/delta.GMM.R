`delta.GMM` <-
function(y, robust=FALSE, c=mean(y), s=sqrt(var(y)), gamma_x=0){
z=(y-c)/s
###### Estimate delta
obj.f=function(delta){
u.d=1/delta*W(delta*z)
if (!robust) s3=skewness(u.d)
else s3=mc(u.d)
(s3-gamma_x)^2
}

lb=-1/exp(1)/max(z)+0.00001
ub=-1/exp(1)/min(z)-0.00001
fit=optimise(f=obj.f, interval=c(lb,ub))
delta.hat=fit$minimum

u=1/delta.hat*W(delta.hat*z)
x=u*s+c

mom.x=c(mean(x), sqrt(var(x)))
c(delta.hat, mom.x)
}

