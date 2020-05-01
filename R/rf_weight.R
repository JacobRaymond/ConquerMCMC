#'MCMC Weighing for the Scaled Subprior Algorithm
#'
#'This function estimates a parameter vector using the scaled subprior approach proposed by Wu and Robert (2019). Using list of Markov chains, it
#'returns an average for the parameters weighted using random forests.
#'@template weighttemplate
#'@param lambda A numeric tuning parameter. The larger its value, the more confidence is placed in the subposterior (see references for details). Must be positive.
#'@returns A vector of parameter estimates.
#'@example examples/rfweight.R
#'@references Changye Wu and Christian P. Robert. Parallelising MCMC via Random Forests. arXiv e-prints, art. arXiv:1911.09698, 2019.
#'@seealso \link[ConquerMCMC]{rf.chains}
#'@export

rf.weight<-function(Chain.Obs, lambda){

  #Define function locally
  `%dopar%` <- foreach::`%dopar%`

  #Extract the parameter names
  para<-names(Chain.Obs[[1]]$Estimates)

  #Extract the number of chains
  chains<-length(Chain.Obs)

  #Verify that the scale factor is valid
  if(lambda <= 0){stop("Scale parameter must be positive.")}

  #Parallelization: training and running random forest models on distinct data sets is embarrassingly parallel
  nCores <- parallel::detectCores(logical = FALSE)
  cl <- parallel::makeCluster(spec = nCores)
  doParallel::registerDoParallel(cl)
  parallel::clusterSetRNGStream(cl = cl)

  i<-NULL

  Chain.Obss <- foreach::foreach(i = 1:chains) %dopar% {

    #Create the observations upon which the random forest will be trained - see Wu and Robert (2019) for the detailed model
    gam<-Chain.Obs[[i]]$`Log-Likelihood`[,1]+(1/chains)*log(Chain.Obs[[i]]$`Log-Likelihood`[,2])
    gam<-gam/lambda
    gam.set<-cbind(Chain.Obs[[i]]$Chains, gam)

    #Create training data; it's a subset (about 10%) of the full observations
    samp<-sample(1:nrow(gam.set), round(0.1*nrow(gam.set)))
    as.data.frame(gam.set[samp,])
  }

  #Train random forest models
  rf <- foreach::foreach(i = 1:chains) %dopar% {
    randomForest::randomForest(gam~., data=Chain.Obss[[i]])
  }

  #Save the length of the "para" vector
  n.par<-length(para)

  #Predict the value of the observations in Chain.Obss using each of the random forest models
  RF.Est <- foreach::foreach(i = 1:chains) %dopar% {
    Predictors<-as.data.frame(Chain.Obss[[i]][,1:n.par])
    colnames(Predictors)<-para
    lapply(rf, function(x) stats::predict(x, newdata=Predictors))}

  #Calculate the weights (another embarassingly parallel task)
  weight<-foreach::foreach(i = 1:chains) %dopar% {
    w<-Reduce(`+`, RF.Est[[i]])-lambda*unlist(RF.Est[[i]][i])
    w/sum(w)
  }
  parallel::stopCluster(cl)

  #Save all of the data into one set
  Chain.Obss<-do.call("rbind", Chain.Obss)

  #Save the weights as a vector
  weight<-Reduce(c, weight)

  #Weighted sum of the parameter estimates
  par.est<-colSums(as.data.frame(Chain.Obss[, 1:n.par]*weight))/chains
  names(par.est)<-para

  return(par.est)
}
