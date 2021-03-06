% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcmc_sub.R
\name{mcmc.sub}
\alias{mcmc.sub}
\title{Metropolis-Hastings Sampler Adapted for Divide-and-Conquer Algorithms}
\usage{
mcmc.sub(
  para,
  startval,
  niter,
  X,
  prior,
  likelihood,
  propvar = NULL,
  burn.rate = 0.1,
  chains = 1,
  num = 1
)
}
\arguments{
\item{para}{Parameters to be estimate.}

\item{startval}{Initial value of the chain.}

\item{niter}{Number of iterations (including burned iterations).}

\item{X}{Matrix of observations from the underlying model.}

\item{prior}{Prior function for the parameters.}

\item{likelihood}{Likelihood function.}

\item{propvar}{The diagonal of the variance matrix for the proposal distribution. If no value is specified, the identify matrix is used.}

\item{burn.rate}{The percentage of iterations to be burned.}

\item{chains}{Number of subsets in the simulation. Used when a divide-and-conquer algorithm is employed}

\item{num}{Numerator of the sub-posterior exponent (Wu and Robert, 2019). Used when a divide-and-conquer algorithm is employed.}
}
\value{
A list with the following items:
\describe{
\item{\code{Chains}}{A dataframe containing the chain iterations that were not burned.}
\item{\code{Estimates}}{The parameter estimates.}
\item{\code{Log-Likelihood}}{A dataframe containing the value of the likelihood function and the prior for each point of the chain.}
}
}
\description{
A basic Metropolis-Hastings sampler for parameter estimation. Includes additional parameters for use with divide-and-conquer methods.
}
\details{
In the divide-and-conquer scheme, the data set \eqn{X} is divided into \eqn{K} subsets \eqn{X_k}. The subposterior for subset \eqn{X_k} is thus
\deqn{\pi_k(\theta | X_k) ∝ (\pi(\theta)^(1/K) \prod p(x|\theta))^\lambda}
See Wu and Robert (2019) for more details.
}
\references{
Changye Wu and Christian P. Robert. Parallelising MCMC via Random Forests. arXiv e-prints, art. arXiv:1911.09698, 2019.
}
