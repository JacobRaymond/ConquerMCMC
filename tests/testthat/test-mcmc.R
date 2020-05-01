require(MCMCpack)
require(MASS)

#Extract locally
MCMCmetrop1R<-MCMCpack::MCMCmetrop1R

test_that("Verify that the MCMC samplers sample from the appropriate logposterior",{

  #Initialization
  set.seed(1234) #The seed is set because the chains were tuned for convergence
  n<-1000
  mu<-rnorm(1, sd=5)

  #Generate a dataset
  X<-rnorm(n, mean=mu)

  #Split the data set (for chain.mcmc)
  X.Split<-workers(X, n=2, random=F)

  #Prior - for simplicity, it's flat
  prior<-function(param){
    1
  }

  #Likelihood function
  loglik<-function(Y, param){
    sum(dnorm(Y, mean=param, log=T))
  }

  #Unnormalized logposterior -  since the prior is flat, it's just the loglikelihood
  loglik.factor<-function(Y) function(param) {
    sum(dnorm(Y, mean=param,  log=T))
  }
  loglik.full<-loglik.factor(X)
  loglik.split1<-loglik.factor(X.Split[[1]])
  loglik.split2<-loglik.factor(X.Split[[2]])

  #Initialize the chains
  param<-c("mean")
  niter<-5000
  startval<-rnorm(1)

  ###Test the regular sampler (mcmc)

  #Run an off-the-shelf sampler, namely MCMCmetrop1R from the MCMCpack package, for every chain in each subset
  #Note: the "capture.output" is due to the fact that MCMCmetrop1R outputs the Metropolis acceptance rate with seemingly no way to suppress it
  mes<-capture.output(mc.full<-MCMCmetrop1R(fun=loglik.full, theta.init = startval, V=as.matrix(0.02), mcmc=niter*0.9, burnin = niter*0.1))

  #Sample using mcmc()
  df.full<-ConquerMCMC::mcmc(para=param, startval=startval, niter=niter, X=X, prior=prior, likelihood=loglik, propvar=0.15)

  #Comparison: we select 100 rows from each logposterior and use a Kolmogorov-Smirnov test to compare them
  # We use a subset because the KS test does not scale well
  # A p-value superior to 0.05 suggests that we cannot reject the null hypothesis that both logposteriors are the same distribution
  row.comp<-sample(1:(niter*0.9), size = 100)
  sim.mc<-mc.full[row.comp]
  sim.df.full<-df.full$Chains[row.comp]
  pval<-suppressWarnings(ks.test(sim.mc, sim.df.full)$p.value)
  expect_true(pval>0.01)

  ###Test the sampler for divide-and-conquer algorithms (mcmc)

  #Sample using mcmc.sub()
  #Setting the numerator equal to the number of chains because we want the unnormalized posterior to be elevated to the power of 1
  df.sub<-ConquerMCMC::mcmc.sub(para=param, startval=startval, niter=niter, X=X, prior=prior, likelihood=loglik, propvar=0.15, chains=2, num=2)

  #Comparison: we select 100 observations from each logposterior and use a Kolmogorov-Smirnov test to compare them
  #We use a subset because the KS test does not scale well
  #A p-value superior to 0.05 suggests that we cannot reject the null hypothesis that both logposteriors are the same distribution
  row.comp<-sample(1:(niter*0.9), size = 100)
  sim.mc<-mc.full[row.comp]
  sim.df.sub<-df.sub$Chains[row.comp]
  pval<-suppressWarnings(ks.test(sim.mc, sim.df.sub)$p.value)
  expect_true(pval>0.01)

  #Comparison: we select 100 observations from each logposterior and use a Kolmogorov-Smirnov test to compare them
  #We use a subset because the KS test does not scale well
  #A p-value superior to 0.05 suggests that we cannot reject the null hypothesis that both logposteriors are the same distribution
  row.comp<-sample(1:(niter*0.9), size = 100)
  sim.mc<-mc.full[row.comp]
  sim.df.full<-df.full$Chains[row.comp]
  pval<-suppressWarnings(ks.test(sim.mc, sim.df.full)$p.value)
  expect_true(pval>0.01)

  ###Test the parallel mcmc sampler (chain.mcmc)

  #Run an off-the-shelf sampler, namely MCMCmetrop1R from the MCMCpack package, for every chain in each subset
  #Note: the "capture.output" is due to the fact that MCMCmetrop1R outputs the Metropolis acceptance rate with seemingly no way to suppress
  mes<-capture.output(mc1<-MCMCmetrop1R(fun=loglik.split1, theta.init = startval, V=as.matrix(0.05), mcmc=niter*0.9, burnin = niter*0.1))
  mes<-capture.output(mc2<-MCMCmetrop1R(fun=loglik.split2, theta.init = startval, V=as.matrix(0.05), mcmc=niter*0.9, burnin = niter*0.1))

  #Sample using chain.mcmc()
  df.chain<-ConquerMCMC::chain.mcmc(chains=2, para=param, startval=startval, niter=niter, X=X, prior=prior, likelihood=loglik, propvar=0.05, burn.rate = 0.1, random=F, num=1)

  #Comparison: we select 100 observations from each logposterior and use a Kolmogorov-Smirnov test to compare them
  #We use a subset because the KS test does not scale well
  #A p-value superior to 0.01 (to account for the simulation variance) suggests that we cannot reject the null hypothesis that both logposteriors are the same distribution
  row.comp<-sample(1:(niter*0.9), size = 100)
  sim.mc1<-mc1[row.comp]
  sim.mc2<-mc2[row.comp]
  sim.df1<-df.chain[[1]]$Chains[row.comp]
  sim.df2<-df.chain[[2]]$Chains[row.comp]
  pval<-suppressWarnings(ks.test(sim.mc1, sim.df1)$p.value)
  expect_true(pval>0.01)
  pval<-suppressWarnings(ks.test(sim.mc2, sim.df2)$p.value)
  expect_true(pval>0.01)}
)
