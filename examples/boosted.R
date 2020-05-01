#Parameter estimation for data from a Weibull distribution

#Prior
prior<-function(param){
  ifelse(all(param>0), 1, 0)
}

#Likelihood
weibull.likelihood<-function(X, param){
  shape=param[1]
  scale=param[2]
  sum(dweibull(x=X, shape, scale, log=TRUE))
}

#Simulate data
X<-rweibull(100, 2, 1.2)

#Parameters
chains<-10
para<-c("shape", "scale")
niter<-10000
startval<-c(1.5, 1)
likelihood=weibull.likelihood

df<-mpost(chains, para, startval, niter, X, prior, likelihood,
          propvar=NULL, burn.rate=0.1, random=TRUE)
