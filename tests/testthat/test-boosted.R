test_that("`boosted()` finds the geometric median and returns the appropriate weighted average",{

  #Initialization
  niter<-10000
  chains<-4 #We assume there are four chains in the model
  param<-c("mu", "sigma")

  Chain.Obs<-vector(mode = "list", length = chains)

  sims.test<-vector(length=10)

  for(j in 1:10){

    #Simulate estimates for the first three chains
    ests<-cbind(rnorm(2, sd=0.5), rnorm(2, sd=0.5))
    ests<-rbind(ests,c(mean(ests[,1]), max(ests[,2])+runif(1, min=1, max=1.5))) #Simulate a third point by construction. Weiszfeld's algorithm does not converge when points are colinear.

    #Create a geometric median artificially
    #The geometric median of four points in two dimensions is the point enclosed within the triangle created by the three other.
    #This is the solution to the four-point Fermat location problem. See https://doi.org/10.1093/imaman/dpl007 theorem 1.
    #The fourth estimate will be the centroid (to reduce the risk of collinearity)
    ests.med<-c(mean(ests[,1]), mean(ests[,2]))
    ests<-rbind(ests, ests.med)

    #Simulate data
    for(i in 1:chains){

      #Simulation results -  not particularly important here, so random numbers are generated
      dat<-cbind(rnorm(n=niter), rnorm(n=niter))
      Chain.Obs[[i]]$Chains<-dat
      colnames(Chain.Obs[[i]]$Chains)<-param

      #Estimates -  random numbers
      Chain.Obs[[i]]$Estimates<-ests[i,]
      names(Chain.Obs[[i]]$Estimates)<-param

      #Loglik and Prior - not important in this case, so random numbers are generated
      Chain.Obs[[i]]$`Log-Likelihood`<-data.frame(matrix(rnorm(n=niter*2), ncol=2))
      colnames(Chain.Obs[[i]]$`Log-Likelihood`)<-c("loglik","prior.vec")
    }

    #Calculate the euclidean norms
    norms<-apply(ests, 1, function(x) sqrt(sum((x-ests.med)^2)))

    #The norm of the median is going to be 0 - to simulate the results of Weiszfeld's algorithm and avoid a denominator of 0, it will be replaced with a very small number.

    norms[4]<-10^runif(1, min =-7, max=-3)

    #Calculate the weights of each estimate
    w<-(1/norms)/sum(1/norms)

    #Weighted average
    sims<-colSums(w*ests)

    #Actual parameter estimation
    par.est<-boosted(Chain.Obs)

    #High tolerance because the Weiszfeld algorithm doesn't always converge
    expect_equal(sum(par.est-sims), 0, tolerance=0.05)
  }
})
