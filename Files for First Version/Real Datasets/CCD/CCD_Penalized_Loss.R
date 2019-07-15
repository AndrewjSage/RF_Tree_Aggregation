#This script produces the penalized loss function information used for table 6 and figure 6 in the paper

#Loss functions
SumLogLik=function(phat, Y){  #Note this version depends on a value of alpha that is defined externally, rather than as an argument
  VAL=NA
  VAL=alpha*log(phat)*Y+log(1-phat)*(1-Y)  #First term penalizes cases where Y=1 by -log(P(Y=1)) (The lower the probability, the worse the penalty since log punishes low numbers the most). Second term punishes cases where Y=0, by 1-P(Y=1). So higher probablity of Y=1 gets penalized more when Y is actually 0. 
  #VAL[phat>0]=log(phat[phat>0])*Y[phat>0]+log(1-phat[phat>0])*(1-Y[phat>0])
  #VAL[phat==0]=-Inf*as.numeric(Y[phat==0]==1)
  #VAL[is.na(VAL)]=0
  return(-sum(VAL))
}
SSE=function(phat, Y){
  return(sum((phat-Y)^2))
}

SphError=function(phat, Y){
  VAL=phat/sqrt((phat^2+(1-phat)^2))*as.numeric(Y==0)+(1-phat)/sqrt((phat^2+(1-phat)^2))*as.numeric(Y==1)
  return(-sum(VAL))
}


BoostingLoss=function(phat, Y){
  VAL=Y*((1-phat)/phat)^(1/2)+(1-Y)*(phat/(1-phat))^(1/2)
  return(sum(VAL))
}


#Find best parameters for each rep and fold
FindBest=function(nreps, nfolds, OOBError){ #For loss func, 1=Log, 2=MSE, 3=Sph,4=Boosting Loss, 5=AUC #should usually use log
  Inds=array(NA, dim=c(nreps, nfolds, 4,2))
  for (rep in 1:nreps){
    for (fold in 1:nfolds){
      for(tech in 1:4)  { #for logloss(for others, change 1 in third index)
        Inds[rep,fold,tech,] = which(OOBError[rep,fold,1,tech,,,3] == min(OOBError[rep,fold,1,tech,,,3]), arr.ind=TRUE)[1,] #1 in 3rd index is for log loss, [1,]pulls off first in case of tie
        #3's in  last index correspond to default nodesize
      }
    }
  }
  return(Inds)
}


library(pROC)
ComputeError=function(TestPreds){
  PredError=array(NA, dim=c(dim(TestPreds)[2],4,3,dim(TestPreds)[5],dim(TestPreds)[1]))
    for(fold in 1:dim(TestPreds)[2]){
      for (mtry in 1:dim(TestPreds)[5]){
        for (ndsize in 1:dim(TestPreds)[1]){
          Predictions=TestPreds[ndsize,fold,,,mtry]
          PredError[fold,1,1,mtry,ndsize]=SumLogLik(Predictions[2,], Predictions[1,]) #RF
          PredError[fold,2,1,mtry,ndsize]=SumLogLik(Predictions[3,], Predictions[1,]) #RFr
          PredError[fold,3,1,mtry,ndsize]=SumLogLik(Predictions[4,], Predictions[1,]) #CF
          PredError[fold,4,1,mtry,ndsize]=SumLogLik(Predictions[5,], Predictions[1,]) #CFa
          PredError[fold,1,2,mtry,ndsize]=SSE(Predictions[2,], Predictions[1,]) #RF
          PredError[fold,2,2,mtry,ndsize]=SSE(Predictions[3,], Predictions[1,]) #RFr
          PredError[fold,3,2,mtry,ndsize]=SSE(Predictions[4,], Predictions[1,]) #CF
          PredError[fold,4,2,mtry,ndsize]=SSE(Predictions[5,], Predictions[1,]) #CFa
          PredError[fold,1,3,mtry,ndsize]=BoostingLoss(Predictions[2,], Predictions[1,])
          PredError[fold,2,3,mtry,ndsize]=BoostingLoss(Predictions[3,], Predictions[1,])
          PredError[fold,3,3,mtry,ndsize]=BoostingLoss(Predictions[4,], Predictions[1,])
          PredError[fold,4,3,mtry,ndsize]=BoostingLoss(Predictions[5,], Predictions[1,])
        }}
      print(fold)
    }
  return(PredError)
}

load("Preds.Rdata")
#dimensions are #1-rep #2-fold #3-Pred Method #4-Case #5-mtry #6-nodesize #7-minbucket

TestPreds=Predictions
OOBPreds=OOBPredictions

library(abind)
#Compute OOBError and PredError for each technique using each mtry, nodesize, and each alpha
alphavec=c(1,1.5,2,3,4,5,10,20,50,100,500,1000)
PredError=array(NA, dim=c(dim(TestPreds)[2],4,3,dim(TestPreds)[5],dim(TestPreds)[1], length(alphavec)))
OOBError=array(NA, dim=c(dim(TestPreds)[2],4,3,dim(TestPreds)[5],dim(TestPreds)[2], length(alphavec)))
for(a in 1:length(alphavec)){
  alpha=alphavec[a]
  PredErrorA=ComputeError(TestPreds)
  OOBErrorA=ComputeError(OOBPreds)
  PredError[,,,,,a]=PredErrorA
  OOBError[,,,,,a]=OOBErrorA
}
#dimensions of PredError and OOBError are 
#1-fold, 2-RF technique, 3=Loss function (just use 1 for Log. Loss with alpha), 4=mtry, 5=nodesize, 6=alpha

#save(PredError, OOBError, file="Error_Penalized_Defmb.Rdata")


###########################################################
#Read in results to create graphic
load("Error_Penalized_Defmb.Rdata")

#For each method and find best choice of mtry, nodesize
FindBest=function(nfolds, OOBError){ #For loss func, 1=Log, 2=MSE, 3=Sph,4=Boosting Loss, 5=AUC #should usually use log
  Inds=array(NA, dim=c( nfolds, 4,2))
  for (fold in 1:nfolds){
    for(tech in 1:4)  {
      Inds[fold,tech,] = which(OOBError[fold,tech,,] == min(OOBError[fold,tech,,]), arr.ind=TRUE)[1,] #1 in 3rd index is for log loss, [1,]pulls off first in case of tie
    }
  }
  return(Inds)
}

#Focus on log Loss
OOBErrors=OOBError[,,1,,,]
PEAll=PredError[,,1,,,]

nfolds=10
#Values of alpha to consider
alphavec=c(1,1.5,2,3,4,5,10,20,50,100,500,1000)


Parameters=array(NA, dim=c(10,4,2,length(alphavec)))   #dims are nfolds, #tech, #tuning par, #alpha
for(a in 1:length(alphavec)){
  Parameters[,,,a]=FindBest(nfolds,OOBErrors[,,,,a])
}
#dimensions are 1-fold, 2-technique, 3-paramter (1=mtry, 2=nodesize), 4=alpha
#Parameters[1,,2,] gives indices of best nodesize parameter for first rep (rows are technique, cols are alpha)

#These are logloss for each nodesize using the best mtry comination from CV for each of 9 folds
CVPredictionErrorbynodesize=array(NA, dim=c(nfolds,4,10,length(alphavec)))
for (a in 1:length(alphavec)){
  ParInds=Parameters[,,,a]
  for (fold in 1:nfolds){
    for(tech in 1:4)  { 
      for(ndsize in 1:16)
        CVPredictionErrorbynodesize[fold, tech, ,a]=PEAll[fold,tech,ParInds[fold,tech,][1],,a]
    }
  }
}
#dimensions are 1-fold, 2-technique, 3-paramter (1=mtry, 2=nodesize), 4=alpha

#average across folds
CVPEN=apply(CVPredictionErrorbynodesize,c(2,3,4),mean)

#Find prediction error from test set correponding to what would have been chosen using cross validation
CVPredictionError=array(NA, dim=c(nfolds,4,length(alphavec)))
for (a in 1:length(alphavec)){
  ParInds=Parameters[,,,a]
  for (fold in 1:nfolds){
    for(tech in 1:4)  { 
      CVPredictionError[fold, tech,a]=PEAll[fold,tech,ParInds[fold,tech,][1],ParInds[fold,tech,][2],a]
    }
  }
}
#dimensions represent 1-fold, 2-technique, 3-nodesize


colMeans(CVPredictionError[,,1])

#Calculate mean prediction error for each alpha and each technique, store in dataframe df2
MeanPE=apply(CVPredictionError, c(2,3), mean)
MinPE=apply(MeanPE, 2,min)
ER=t(apply(MeanPE, 1, function(x) x/MinPE))
Mthd=c(rep(c("RFc", "RFr", "CF", "CFa"),length(alphavec)))
df2=data.frame(rep(alphavec, each=4), Mthd,c(ER))
names(df2)=c("Alpha", "Method", "Error")

#Display loss function values, rows are techniques, columns are alpha
PenError=MeanPE[2:4,]

#Create and plot ratio of prenalized error for one technique, relative to best for given alpha
#Function to divide each row by min from each row.
ratiofunc=function(x){ #input is a vector
return(x/min(x))  
}

#compute ratio of error for each technique to best for a given alpha
PERatio=apply(PenError, 2,ratiofunc)
Pendf=data.frame(rep(alphavec,3), c(PERatio[1,], PERatio[2,], PERatio[3,]), c(rep(c("RF-P", "CF-WP", "CF-P"), each=12)))
names(Pendf)=c("Alpha", "Ratio", "Method")

#save(Pendf, file="Pendf_CCD.Rdata")
