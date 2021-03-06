% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Boosted.R
\name{boosted}
\alias{boosted}
\title{MCMC Weighing with the M-Posterior Method}
\usage{
boosted(Chain.Obs)
}
\arguments{
\item{Chain.Obs}{A nested list of MCMC run on subsets. The length of the list corresponds to the number of chains.
Each unit correspond to a list for one of the subsets, composted of three elements:
\describe{
\item{\code{Chains}}{A Markov Chain.}
\item{\code{Estimates}}{A vector parameter estimate obtained using the MCMC in "Chains"}
\item{\code{Log-Likelihood}}{A two dimensional vector. One dimension, called "loglik", contains the loglikelihood evaluated
at every point of the chain. "prior.vec", meanwhile, is the value of the prior at each point of the chain.}
}}
}
\value{
A vector of parameter estimates.
}
\description{
This function estimates a parameter vector using the m-posterior approach proposed by Minsker et al. (2014). Using list of MCMC, it
returns an average weighted using the geometric median.
}
\examples{
#Parameter estimation for data from a normal distribution

#Prior
prior<-function(param){
  ifelse(all(param>0), 1, 0)
}

#Likelihood function
normal.likelihood<-function(X, param){
  mu=param[1]
  sigma=param[2]
  sum(dnorm(x=X, mean=mu, sd=sigma, log=TRUE))
}

#Simulate data
X<-rnorm(100, 2, 1.2)

#Parameters
param<-c("mu", "sigma")
niter<-10000
startval<-c(1, 1)
chains<-4

#Simulate "Chain.Obs"
Chain.Obs<-chain.mcmc(chains,param, startval, niter=niter, X=X, prior=prior,
               likelihood=normal.likelihood, propvar=0.25, random=TRUE, num=1)

#Weigh the observations
df<-rf.weight(Chain.Obs, lambda = 1)
}
\references{
Stanislav Minsker, Sanvesh Srivastava, Lizhen Lin, and David B. Dunson. Scalable and robust bayesian inference via the median posterior. In Proceedings of the 31st International Conference on International Conference on Machine Learning - Volume 32, ICML’14, page II–1656–II–1664. JMLR.org, 2014.
}
\seealso{
\link[ConquerMCMC]{mpost}
}
