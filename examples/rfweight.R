#Parameter estimation for data from a normal distribution

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
chains<-4

#Simulate "Chain.Obs"
Chain.Obs<-chain.mcmc(chains,param, startval, niter=niter, X=X, prior=prior,
               likelihood=normal.likelihood, propvar=0.25, random=TRUE, num=1)

#Weigh the observations
df<-rf.weight(Chain.Obs, lambda = 1)
