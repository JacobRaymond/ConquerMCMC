#' Metropolis-Hastings Sampler Adapted for Divide-and-Conquer Algorithms
#'
#' A basic Metropolis-Hastings sampler for parameter estimation. Includes additional parameters for use with divide-and-conquer methods.
#' @template MCMCSubTemplate
#' @return A list with the following items:
#' \describe{
#'\item{`Chains`}{A dataframe containing the chain iterations that were not burned.}
#'\item{`Estimates`}{The parameter estimates.}
#'\item{`Log-Likelihood`}{A dataframe containing the value of the likelihood function and the prior for each point of the chain.}
#' }
#'
#' @export
mcmc.sub = function( para, startval, niter, X, prior, likelihood, propvar=NULL, burn.rate=0.1, chains=1, num=1){

  #Verify that the starting point is valid
  if(prior(startval)==0 || is.na(likelihood(X, startval))){stop("Invalid Starting Point")}

  #Verify that the burn rate is valid
  if(burn.rate>1 || burn.rate <0){stop("Burn rate must be between 0 and 1")}

  #Verify that "chains" is appropriate
  if(chains<0 || chains%%1 !=0){stop("Chains must be a positive integer.")}

  #Calculate number of iterations to be burned
  burnit<-round(niter*burn.rate)

  #Create variance matrix for the proposal distribution
  if(!is.null(propvar)){propvar=propvar^2*diag(length(para))}

  #Burned iterations
  #Begin at the starting value
  current<-startval
  likelihood.current=likelihood(X, startval)
  prior.current=log(prior(startval))

  #Denominator - see details
  dem=1/chains

  #Begin iterations
  for(i in 2:burnit){

    #New candidate - we use random walk Metropolis
    new = mniw::rmNorm(1, mu=current, Sigma=propvar)

    #Calculate the likelihood of the proposed value
    likelihood.new=likelihood(X, new)

    #Skip if the proposed value is not valid
    if(!is.na(likelihood.new)){

      #Calculate the log of the prior
      prior.new=log(prior(new))

      #Acceptance ratio
      A = num*(dem*(prior.new-prior.current)+(likelihood.new-likelihood.current))

      #Determine whether to accept the proposed value
      if(A>0 || stats::runif(1)<exp(A)){

        #Change the "current" likelihood and prior if the new value is accepted.
        likelihood.current=likelihood.new
        prior.current=prior.new
        current=new
      }
    }
  }

  #Matrix to preserve the unburnt observations
  obs<-matrix(ncol=length(para), nrow=(niter-burnit))
  colnames(obs)<-para

  #Vectors to house the likelihoods and priors
  loglik<-vector(length = (niter-burnit))
  prior.vec<-vector(length = (niter-burnit))

  #Begin the chain at the last burned observation
  obs[1,] = current
  prior.vec[1]<-prior.current
  loglik[1]<-likelihood.current

    #Begin iterations
    for(i in 2:(niter-burnit)){

      #New candidate - we use random walk Metropolis
      new = mniw::rmNorm(1, mu=obs[i-1,], Sigma=propvar)

      #Calculate the likelihood of the proposed value
      likelihood.new<-likelihood(X, new)

      #Skip if the proposed value is not valid
      if(!is.na(likelihood.new)){

        #Calculate the log of the prior
        prior.new<-log(prior(new))

        #Acceptance Ratio
        A = num*(dem*(prior.new-prior.vec[i-1])+likelihood.new-loglik[i-1])

        #Determine whether to accept the proposed value
        if(A>0 || stats::runif(1)<exp(A)){

          #Change the "current" likelihood and prior if the new value is accepted
          obs[i,] = new
          loglik[i]<-likelihood.new
          prior.vec[i]<-prior.new

        }else{obs[i,] = obs[i-1,]

        #Store the old observations if the new value was rejected
        loglik[i]=loglik[i-1]
        prior.vec[i]<-prior.vec[i-1]}

      }else{

      #If the candidate is not suitable - use the previous value
      obs[i,] = obs[i-1,]
      loglik[i]=loglik[i-1]
      prior.vec[i]<-prior.vec[i-1]}
    }

  #Take the exponent of the log of the vectors
  prior.vec<-exp(prior.vec)

  #Output results
  output<-list(obs, colMeans(obs), cbind(loglik, prior.vec))
  names(output)<-c("Chains", "Estimates", "Log-Likelihood")
  return(output)
}




