#' Parallel MCMC on Subsets
#'
#' Takes a dataset X, subdivides it into an inputted number of subsets, and runs an MCMC on each subset in parallel.
#' @template MCMCSubTemplate
#' @param random If true, the rows of X are shuffled prior to the split.
#' @example examples/parallel.R
#'
#' @return Returns a nested list. Each element in the list corresponds to an output from \code{\link{mcmc.sub}}.
#'
#' @export
chain.mcmc<-function(chains, para, startval, niter, X, prior, likelihood, propvar=NULL, burn.rate=0.1, random=T, num){

  #Define function locally
  `%dopar%` <- foreach::`%dopar%`

  #Verifies that "chain" is valid.
  if(chains<1){stop("Chains must be a positive number.")}

    #Creates a list of subsets of X
    X.set<-workers(X, n=chains, random = random)

    #Parallelization: running MCMC on distinct datasets is embarrassingly parallel.
    Chain.Obs<-vector(mode = "list", length = chains)
    nCores <- parallel::detectCores(logical = FALSE)
    cl <- parallel::makeCluster(spec = nCores)
    doParallel::registerDoParallel(cl)
    parallel::clusterSetRNGStream(cl = cl)

    j<-NULL

    #Run an MCMC on each subset
    Chain.Obs <- foreach:: foreach(j = 1:chains) %dopar% {
      ConquerMCMC::mcmc.sub(para, startval, niter=niter, X.set[[j]], prior, likelihood, propvar, burn.rate, chains, num)}

  #De-allocate parallel ressources
  parallel::stopCluster(cl)

  #Output
  Chain.Obs
}

