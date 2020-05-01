#'MCMC Weighing with the Consensus Monte Carlo Method
#'
#'This function estimates a parameter vector using the consensus Monte Carlo approach proposed by Scott et al. (2016). Using list of MCMC, it
#'returns a weighted average of parameters.
#'@template weighttemplate
#'@returns A vector of parameter estimates.
#'@example examples/consensus.R
#'@references Steven L. Scott, Alexander W. Blocker, Fernando V. Bonassi, Hugh A. Chipman, Edward I. George, and Robert McCulloch. Bayes and big data: The consensus monte carlo algorithm. International Journal of Management Science and Engineering Management, 11(2):78–88, 2016.
#'@seealso \link[ConquerMCMC]{rf.chains}
#'@export

consensus<-function(Chain.Obs){

  #Extract the parameter names
  para<-names(Chain.Obs[[1]]$Estimates)

  #Extract the number of chains
  chains<-length(Chain.Obs)

  #Extract the chains
  chain.list<-lapply(Chain.Obs, `[[`, 1)

  #Extract estimates and vectorize them
  ests<-lapply(Chain.Obs, `[[`, 2)
  ests<- matrix(unlist(ests), ncol=length(para), byrow=T)
  ests<-as.vector(ests)

  #Calculate the inverse standard deviation of each chain and vectorize them
  sds<-lapply(chain.list, function(x) apply(x, 2, stats::var))
  sds<-matrix(unlist(sds), ncol=length(para), byrow=T)
  sds<-as.vector(sds)

  #Create a sparse matrix. It will be used to calculate the weights
  sparse<-suppressWarnings(matrix( c(rep(1, chains), rep(0,chains*length(para))), byrow=TRUE, nrow=length(para), ncol=chains*length(para)))

  #Weight each estimate according to the standard deviation of the chains
  W<-sparse%*%diag(sds)
  num<-W%*%ests
  den<-rowSums(W)

  #Parameter estimate
  par.est<-as.numeric(num/den)
  return(par.est)
}
