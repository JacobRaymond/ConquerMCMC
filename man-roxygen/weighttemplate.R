#' @param Chain.Obs A nested list of MCMC run on subsets. The length of the list corresponds to the number of chains.
#' Each unit correspond to a list for one of the subsets, composted of three elements:
#' \describe{
#' \item{`Chains`}{A Markov Chain.}
#' \item{`Estimates`}{A vector parameter estimate obtained using the MCMC in "Chains"}
#' \item{`Log-Likelihood`}{A two dimensional vector. One dimension, called "loglik", contains the loglikelihood evaluated
#' at every point of the chain. "prior.vec", meanwhile, is the value of the prior at each point of the chain.}
#' }
#'
