#'Consensus Algorithm for Parameter Estimation
#'
#'This function estimates the paraeters of a model using the consensus algorithm method developped by Scott et al. (2016). The consensus algorithm models the subposterior of subset \eqn{X_k} as
#' \deqn{\pi_k(\theta | X_k) ∝ \pi(\theta)^(1/K) \prod p(x|\theta)}
#' Parameter estimates are subsequently weighted according to the variance of their Markov chains.
#' @template DACTemplate
#' @return A list with the following items:
#' \describe{
#'\item{`Chains`}{A list of dataframes, one for each subset, containing the generated Markov chains.}
#'\item{`Estimate`}{The paraeter estimate obtained using the consensus algorithm}
#' }
#' @example examples/subprior.R
#' @references Steven L. Scott, Alexander W. Blocker, Fernando V. Bonassi, Hugh A. Chipman, Edward I. George, and Robert McCulloch. Bayes and big data: The consensus monte carlo algorithm. International Journal of Management Science and Engineering Management, 11(2):78–88, 2016.
#' @export

subprior<-function(chains, para, startval, niter, X, prior, likelihood, propvar=NULL, burn.rate=0.1, random=T){

  #Run chains on each subset
  #num=1 because this is a subposterior method
  Chain.Obs<-chain.mcmc(chains, para, startval, niter, X, prior, likelihood, propvar, burn.rate, random, num=1)

  #Extract the chains
  chain.list<-lapply(Chain.Obs, `[[`, 1)

  #Parameter Estimation
  par.est<-ConquerMCMC::consensus(Chain.Obs)

  #Output
  Output=list(chain.list, par.est)
  names(Output)<-c("Chains", "Estimate")
  return(Output)
}
