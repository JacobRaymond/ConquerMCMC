#' @param propvar The diagonal of the variance matrix for the proposal distribution. If no value is specified, the identify matrix is used.
#' @param burn.rate The percentage of iterations to be burned.
#' @param chains Number of subsets in the simulation. Used when a divide-and-conquer algorithm is employed.
#' @param para Parameters to be estimated.
#' @param startval Initial value of the chain.
#' @param niter Number of iterations (including burned iterations).
#' @param X Matrix of observations from the underlying data set.
#' @param prior Prior function for the parameters.
#' @param likelihood Likelihood function.
#' @param random If true, the rows of X are shuffled prior to the split.

