#Parameter estimation for data from a normal distribution

#Prior
prior<-function(param){
  ifelse(param[2]>0, 1, 0)
}

#Likelihood
normal.likelihood<-function(X, param){
  mu=param[1]
  sigma=param[2]
  sum(dnorm(x=X, mean=mu, sd=sigma, log=T))
}

#Simulate data
X<-rnorm(100, -0.5, 0.5)

#Parameters
param<-c("mu", "sigma")
niter<-2500
startval<-c(0, 5)

df<-subprior(chains=4,para=param, startval=startval, niter=niter, X=X,
             prior=prior, likelihood=normal.likelihood, propvar=0.25, random=TRUE)

