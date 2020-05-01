#' @param propvar The diagonal of the variance matrix for the proposal distribution. If no value is specified, the identify matrix is used.
#' @param burn.rate The percentage of iterations to be burned.
#' @param chains Number of subsets in the simulation. Used when a divide-and-conquer algorithm is employed
#' @param num Numerator of the sub-posterior exponent (Wu and Robert, 2019). Used when a divide-and-conquer algorithm is employed.
#' @param para Parameters to be estimate.
#' @param startval Initial value of the chain.
#' @param niter Number of iterations (including burned iterations).
#' @param X Matrix of observations from the underlying model.
#' @param prior Prior function for the parameters.
#' @param likelihood Likelihood function.
#' @details In the divide-and-conquer scheme, the data set \eqn{X} is divided into \eqn{K} subsets \eqn{X_k}. The subposterior for subset \eqn{X_k} is thus
#' \deqn{\pi_k(\theta | X_k) ∝ (\pi(\theta)^(1/K) \prod p(x|\theta))^\lambda}
#' See Wu and Robert (2019) for more details.
#' @references Changye Wu and Christian P. Robert. Parallelising MCMC via Random Forests. arXiv e-prints, art. arXiv:1911.09698, 2019.
