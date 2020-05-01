#Parameter Estimation for Data from a Normal Distribution

#Prior
prior<-function(param){
  ifelse(all(param>0), 1, 0)
}

#Likelihood function
normal.likelihood<-function(X, param){
  mu=param[1]
  sigma=param[2]
  sum(dnorm(x=X, mean=mu, sd=sigma, log=TRUE))
}

#Simulate data
X<-rnorm(100, 2, 1.2)

#Parameters
param<-c("mu", "sigma")
niter<-10000
startval<-c(1, 1)

df<-mcmc.sub(param, startval, niter, X, prior, normal.likelihood, propvar=0.25)
