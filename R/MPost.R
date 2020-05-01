#'M-Posterior Algorithm for Parameter Estimation
#'
#'This function estimates the parameters of a model using the M-posterior method developped by Minsker et al. (2014). It models the subposterior of subset \eqn{X_k} as
#' \deqn{\pi_k(\theta | X_k) ∝ \pi(\theta) (\prod p(x|\theta))^K}
#' Parameter estimates are subsequently weighted according to their geometric mean.
#' @template DACTemplate
#' @return A list with the following items:
#' \describe{
#'\item{`Chains`}{A list of dataframes, one for each subset, containing the generated Markov chains.}
#'\item{`Estimate`}{The parameter estimate obtained using the M-posterior method.}
#' }
#' @example examples/boosted.R
#' @references Stanislav Minsker, Sanvesh Srivastava, Lizhen Lin, and David B. Dunson. Scalable and robust bayesian inference via the median posterior. In Proceedings of the 31st International Conference on International Conference on Machine Learning - Volume 32, ICML’14, page II–1656–II–1664. JMLR.org, 2014.
#' @export

mpost<-function(chains, para, startval, niter, X, prior, likelihood, propvar=NULL, burn.rate=0.1, random=T){

  #Run chains on each subset
  #num=chains because we are employing a boosted subposterior method
  Chain.Obs<-chain.mcmc(chains, para, startval, niter, X, prior, likelihood,  propvar, burn.rate, random, num=chains)

  #Extract the chains
  chain.list<-lapply(Chain.Obs, `[[`, 1)

  #Extract parameter estimates
  par.est<-ConquerMCMC::boosted(Chain.Obs)

  #Output
  Output<-list(chain.list, par.est)
  names(Output)<-c("Chains", "Estimate")
  return(Output)
}

