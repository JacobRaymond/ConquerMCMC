test_that("`workers()` splits a data set",{

  for(i in 1:10){

  #Generate a size for the simulated dataset
  size.data<-sample(1000:25000, size = 1)

  #Generate a large dataset
  X<-data.frame(cbind(runif(size.data),runif(size.data),runif(size.data)))

  #Number of data sets to split the data into
  n<-sample(2:10, size = 1)

  #Split data
  X.split<-workers(X, n, random=T)

  #Check whether a randomly chosen row from the orignal data set appears in one of the split sets
  test.value<-sample(1:size.data, size=1)
  match.test<-lapply(X.split, function(y) nrow(merge(X[test.value,],y))>0)
  result<-suppressWarnings(any(match.test))
  expect_true(result)}
})

