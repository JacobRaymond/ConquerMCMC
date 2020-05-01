test_that("`consensus()` returns the appropriate weighted average",{

  niter<-10000 #We assume this is post-burn
  param<-"mu"

  for(i in 1:10){

    #Initialization

    chains<-sample(2:10, 1)

    #Simulate standard deviations
    stdev<-runif(n = chains, min=0.01, max=10)

    #Simulate means
    means<-rnorm(n=chains)

    Chain.Obs<-vector(mode = "list", length = chains)
    #Simulate data
    for(i in 1:chains){

      #Chains -  each chain will have an exact mean and standard deviation
      dat<-rnorm(n=niter)
      Chain.Obs[[i]]$Chains<-as.data.frame(stdev[i]*((dat-mean(dat))/sd(dat))+means[i])
      colnames(Chain.Obs[[i]]$Chains)<-param

      #Estimates
      Chain.Obs[[i]]$Estimates<-means[i]
      names(Chain.Obs[[i]]$Estimates)<-param

      #Loglik and Prior - not important in this case, so will simulate random numbers
      Chain.Obs[[i]]$`Log-Likelihood`<-data.frame(matrix(rnorm(n=niter*2), ncol=2))
      colnames(Chain.Obs[[i]]$`Log-Likelihood`)<-c("loglik","prior.vec")

    }

    #Extract the estimates
    par.est<-consensus(Chain.Obs)

    #Calculate the weights
    var<-stdev^2
    wavg<-sum(var*means)/sum(var)

    expect_equal(sum(par.est-wavg), 0)
  }
})

