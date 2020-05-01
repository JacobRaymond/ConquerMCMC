require(mniw)

test_that("The random forest algorithm returns values that are close to the MLE",{
  #This test will assess the performance of the random forest-based algorithm by comparing its output to a known estimator, the MLE.

  for(i in 1:5){

    #Generate data
    n<-sample(1000:10000, 1)
    sigma<-runif(1, min=0.01, max=10)
    mu<-rnorm(1, sd=5)
    X<-rnorm(n, mean=mu, sd =sigma)

    #Flat Prior
    prior<-function(param){
      ifelse(param[2]>0, 1, 0)
    }

    #Likelihood function
    loglik<-function(X, param){
      mu=param[1]
      sigma=param[2]
      sum(dnorm(x=X, mean=mu, sd=sigma, log=TRUE))
    }

    #Parameters
    para<-c("mu", "sigma")
    niter<-sample(7500:15000, 1)
    startval<-runif(2)
    chains<-sample(2:10, 1)
    lambda<-sample(1:chains, 1)

    #Run the chain
    df<-ConquerMCMC::rf.chains(lambda, chains, para, startval, niter, X, prior,
                  likelihood=loglik, propvar=1)

    #Extract the estimates
    df.ests<-df$Estimate

    #Extract the MLE
    mle<-c(mean(X), sd(X))

    #Calculate the difference between the estimates and MLE
    diff<-abs(df.ests-mle)

    #Verify that the estimates are fairly close (tolerance: 0.05)
    expect_true(all(diff<0.05))
  }
})


