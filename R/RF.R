#' Scaled Subposterior Parallelization for Parameter Estimation
#'
#'This function estimates the parameters of a model using the scaled subposterior approach proposed by Wu and Robert (2019). It models the subposterior of subset \eqn{X_k} as
#' \deqn{\pi_k(\theta | X_k) ∝ (\pi(\theta)^(1/K) \prod p(x|\theta))^\lambda}
#' where \eqn{\lambda} is a tuning parameter. The estimates are then averaged using importance sampling and random forests.
#' @template DACTemplate
#' @param lambda A numeric tuning parameter. The larger its value, the more confidence is placed in the subposterior (see references for details). Must be positive.
#' @return A list with the following items:
#' \describe{
#'\item{`Chains`}{A list of dataframes, one for each subset, containing the generated Markov chains.}
#'\item{`Estimate`}{The parameter estimate obtained using the scaled subposterior method.}
#' }
#' @example examples/rf.R
#' @references Changye Wu and Christian P. Robert. Parallelising MCMC via Random Forests. arXiv e-prints, art. arXiv:1911.09698, 2019.
#' @export

rf.chains<-function(lambda,chains,para, startval, niter, X, prior, likelihood, propvar, random=T, burn.rate=0.1){

  #Define function locally
  `%dopar%` <- foreach::`%dopar%`

  #Verify that the scale factor is valid
  if(lambda <= 0){stop("Scale parameter must be positive.")}

  #Parallel; "num" is the scale parameter
  df<-chain.mcmc(chains, para, startval, niter, X, prior, likelihood, propvar, burn.rate, random, num=lambda)

  #Extract the chains
  chain.list<-lapply(df, `[[`, 1)

  #Extract the estimates
  par.est<-rf.weight(df, lambda)

  #Output
  Output<-list(chain.list, par.est)
  names(Output)<-c("Chains", "Estimate")
  return(Output)
}

