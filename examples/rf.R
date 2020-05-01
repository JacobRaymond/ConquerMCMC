#Parameter Estimation for Data from a Weibull Distribution

#Prior
prior<-function(param){
  ifelse(all(param>0), 1, 0)
}

#Likelihood
weibull.likelihood<-function(X, param){
  shape=param[1]
  scale=param[2]
  sum(dweibull(x=X, shape, scale, log=T))
}

#Simulate data
X<-rweibull(100, 2, 1.2)

#Parameters
para<-c("shape", "scale")
niter<-10000
startval<-c(2.5, 1.1)
chains=3
burn.rate<-0.1
propvar<-0.5
lambda<-2

df<-rf.chains(lambda, chains, para, startval, niter, X, prior,
              likelihood=weibull.likelihood, propvar=propvar)
