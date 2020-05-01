#' Segment a data set into subsets
#'
#' This algorithm segments a data set into a prespecified number of subsets.
#' @param X A matrix to be subsetted.
#' @param n Number of subsets to be created.
#' @param random If true, the rows of X are shuffled prior to the split.
#' @return A list containing the n subsets.
#' @examples
#' #Split a matrix with 100 rows and two columns into 5 subsets
#'
#' #Simulate data
#' df<-matrix(data = c(rnorm(100), runif(100)), ncol=2)
#'
#' #Return list of subsets
#' workers(df, n=5)
#'
#' @export
workers <- function(X,n, random=T){
  if(random==T){

    #Split the data into intervals
    split(X, cut(sample(1:NROW(X)), n, labels = F)) }

  else{

    #Split the data into intervals without shuffling the rows of X
    split(X, cut(1:length(X), n, labels = F))
    }
  }

