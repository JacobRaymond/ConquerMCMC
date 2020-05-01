#'MCMC Weighing with the M-Posterior Method
#'
#'This function estimates a parameter vector using the m-posterior approach proposed by Minsker et al. (2014). Using list of MCMC, it
#'returns an average weighted using the geometric median.
#'@template weighttemplate
#'@returns A vector of parameter estimates.
#'@example examples/rfweight.R
#'@references Stanislav Minsker, Sanvesh Srivastava, Lizhen Lin, and David B. Dunson. Scalable and robust bayesian inference via the median posterior. In Proceedings of the 31st International Conference on International Conference on Machine Learning - Volume 32, ICML’14, page II–1656–II–1664. JMLR.org, 2014.
#'@seealso \link[ConquerMCMC]{mpost}
#'@export

boosted<-function(Chain.Obs){

  #Extract the parameter names
  para<-names(Chain.Obs[[1]]$Estimates)

  #Extract the estimates from each chain
  ests<-lapply(Chain.Obs, `[[`, 2)
  ests<-matrix(unlist(ests), ncol = length(para), byrow = TRUE)

  #Calculate the geometric median of the estimates
  geomed<-suppressWarnings(pracma::geo_median(ests)$p)

  #Calculate the weights for each estimate
  w.norm<-apply(ests, 1, function(x) pracma::Norm(x-geomed))
  w<-(1/w.norm)/sum(1/w.norm)

  #Reweighing
  w[w <1/(2*length(para))]<-0
  w<-w/sum(w)

  #Weighted sum of the parameter estimates
  par.est<-colSums(w*ests)
  names(par.est)<-para

  return(par.est)
}
