#This script reads in the predictions and produces summary information used 
#to create the graphics in the paper.

load("Preds.Rdata")
#Indices on OOBPreds and TestPreds are
#1-rep #2-fold #3-Pred Method #4-Case #5-mtry #6-nodesize #7-minbucket

###################################################################################################
#Different loss functions. We calculate all of them, even though only use LogLoss in paper

#Calculate sum of binomial log loss error for set of predicted probabilities
SumLogLik=function(phat, Y){
  VAL=NA
  VAL=log(phat)*Y+log(1-phat)*(1-Y)  #First term penalizes cases where Y=1 by -log(P(Y=1)) (The lower the probability, the worse the penalty since log punishes low numbers the most). Second term punishes cases where Y=0, by 1-P(Y=1). So higher probablity of Y=1 gets penalized more when Y is actually 0. 
  return(-sum(VAL))
}

#Calculates SSE for set of predicted probabilities
SSE=function(phat, Y){
  return(sum((phat-Y)^2))
}

#Calculates spherical loss for set of predicted probabilities
SphError=function(phat, Y){
  VAL=phat/sqrt((phat^2+(1-phat)^2))*as.numeric(Y==0)+(1-phat)/sqrt((phat^2+(1-phat)^2))*as.numeric(Y==1)
  return(-sum(VAL))
}

#Calculates Boosting Loss for set of predicted probabilities
BoostingLoss=function(phat, Y){
  VAL=Y*((1-phat)/phat)^(1/2)+(1-Y)*(phat/(1-phat))^(1/2)
  return(sum(VAL))
}
#######################################################################################################

#Find best parameters for each rep and fold using OOB predictions
FindBest=function(nfolds, OOBError){ #For loss func, 1=Log, 2=MSE, 3=Sph,4=Boosting Loss, 5=AUC #should usually use log
  Inds=array(NA, dim=c(nfolds, 4,3))
    for (fold in 1:nfolds){
      for(tech in 1:4)  { #for logloss(for others, change 1 in third index)
        Inds[fold,tech,] = which(OOBError[fold,1,tech,,,] == min(OOBError[fold,1,tech,,,]), arr.ind=TRUE)[1,] #1 in 3rd index is for log loss, [1,]pulls off first in case of tie
      }
    }
  return(Inds)
}


#Computes error for each of the above loss functions for each fold, mtry, and nodesize comb.
#Can be run on OOB or predicted probs
ComputeError=function(TestPreds){
PredError=array(NA, dim=c(dim(TestPreds)[2], 4,3,dim(TestPreds)[5],dim(TestPreds)[1] )) #index of 4 is for 4 different prediction techniques, and index of 3 is for 3 loss functions
  for(fold in 1:dim(TestPreds)[2]){
    for (mtry in 1:dim(TestPreds)[5]){
      for (ndsize in 1:dim(TestPreds)[1]){
    Predictions=TestPreds[ndsize,fold,,,mtry]
    PredError[fold,1,1,mtry,ndsize]=SumLogLik(Predictions[2,], Predictions[1,]) #RFclassification (not used in paper)
    PredError[fold,2,1,mtry,ndsize]=SumLogLik(Predictions[3,], Predictions[1,]) #RF regression
    PredError[fold,3,1,mtry,ndsize]=SumLogLik(Predictions[4,], Predictions[1,]) #CF proportional weighting
    PredError[fold,4,1,mtry,ndsize]=SumLogLik(Predictions[5,], Predictions[1,]) #CF equal weighting
    PredError[fold,1,2,mtry,ndsize]=SSE(Predictions[2,], Predictions[1,]) #RFclassification (not used in paper)
    PredError[fold,2,2,mtry,ndsize]=SSE(Predictions[3,], Predictions[1,]) #RF regression
    PredError[fold,3,2,mtry,ndsize]=SSE(Predictions[4,], Predictions[1,]) #CF proportional weighting
    PredError[fold,4,2,mtry,ndsize]=SSE(Predictions[5,], Predictions[1,]) #CF equal weighting
    PredError[fold,1,3,mtry,ndsize]=BoostingLoss(Predictions[2,], Predictions[1,]) #RFclassification (not used in paper)
    PredError[fold,2,3,mtry,ndsize]=BoostingLoss(Predictions[3,], Predictions[1,]) #RF regression
    PredError[fold,3,3,mtry,ndsize]=BoostingLoss(Predictions[4,], Predictions[1,]) #CF proportional weighting
    PredError[fold,4,3,mtry,ndsize]=BoostingLoss(Predictions[5,], Predictions[1,]) #CF equal weighting
        }}
    print(fold)
    }
return(PredError)
}


#Computes AUC for each of the above loss functions for each fold, mtry, and nodesize comb.
#Can be run on OOB or predicted probs
library(pROC)
ComputeAUC=function(TestPreds){
  AUC=array(NA, dim=c(dim(TestPreds)[2], 4,dim(TestPreds)[5],dim(TestPreds)[1] ))
  for(fold in 1:dim(TestPreds)[2]){
    for (mtry in 1:dim(TestPreds)[5]){
      for (ndsize in 1:dim(TestPreds)[1]){
        Predictions=TestPreds[ndsize,fold,,,mtry]
           AUC[fold,1,mtry,ndsize]=auc(Predictions[1,], Predictions[2,]) #RFclassification (not used in paper)
            AUC[fold,2,mtry,ndsize]=auc(Predictions[1,], Predictions[3,])  #RF regression
            AUC[fold,3,mtry,ndsize]=auc(Predictions[1,], Predictions[4,]) #CF proportional weighting
            AUC[fold,4,mtry,ndsize]=auc(Predictions[1,], Predictions[5,]) #CF equal weighting
          }}
      print(fold)
      }
  return(AUC)
}

#Dimensions of Predictions are #1-fold, #2-technique, #3-lossfunction, #4-mtry, #5-nodesize

#Compute Loss for each technique on test cases
PredError=ComputeError(Predictions)
save(PredError, file="PredError.Rdata")

#Compute loss for each technique using OOB training cases
OOBError=ComputeError(OOBPredictions)
save(OOBError, file="OOBErrors.Rdata")


#Compute AUC on test cases
AUC=ComputeAUC(TestPreds)
save(AUC, file="AUC.Rdata")


###########################################################
#Create graphics using information about errors
load("OOBErrors.Rdata")
load("PredError.Rdata")
load("AUC.Rdata")
load("Preds.Rdata")

#Dimensions of PredError, OOBErrors, PredAUC, AUC are
#1-rep 2-fold 3-method 4-loss func 5-mtry 6-nodesize 7-minb

TestPreds=Predictions

#For each method and find best choice of mtry, nodesize using OOB cases
#We exclusively use logloss from now on. Change third index as described below to use other loss function
FindBest=function(nfolds, OOBError){ #For loss func, 1=Log, 2=MSE, 3=Sph,4=Boosting Loss, 5=AUC #should usually use log
  Inds=array(NA, dim=c( nfolds, 4,2)) #4 techniques, #2 tuning parameters
  for (fold in 1:nfolds){
    for(tech in 1:4)  { #for logloss(for others, change 1 in third index)
      Inds[fold,tech,] = which(OOBError[fold,tech,1,,] == min(OOBError[fold,tech,1,,]), arr.ind=TRUE)[1,] #1 in 3rd index is for log loss, [1,]pulls off first in case of tie
    }
  }
  return(Inds)
}

#ParInds contain indices of best mtry, nodesize combination for each of 10 folds.
ParInds=FindBest(10,OOBError)

#Get rid of first dim since there is only 1 rep
PEAll=PredError
PEAll=PEAll[,,1,,]  #Just use Logloss

#Get preds resulting from best mtry (dims 3)
PE=apply(PEAll,c(1,2,4),min)
#Dims are 1-fold 2-tech 3-nodesize
#Average across folds. This gives best performance for each technique, nodesize combination on 
#test data. This is NOT the pred. error corresponding to the parameters determined by CV.
PE=apply(PE,c(2,3),mean)
#Get best AUCs as a function of nodesize (max over mtry (dims 3))
#This is the best performance on the test data, not the best as predicted by CV.
bestAUC=apply(AUC,c(1,2,4),min)
bestAUC=apply(bestAUC,c(2,3),mean)


nfolds=10
#These are logloss for each nodesize using the best mtry from CV for each of 10 folds
CVPredictionErrorbynodesize=array(NA, dim=c(nfolds,4,10))
for (fold in 1:nfolds){
  for(tech in 1:4)  { 
    for(ndsize in 1:10)
      CVPredictionErrorbynodesize[fold, tech, ]=PEAll[fold,tech,ParInds[fold,tech,][1],]
  }
}
#dimensions are fold by prediction type by nodesize
#CVPredictionErrorbynodesize[,1,] gives results for RF classification
#CVPredictionErrorbynodesize[,2,] gives results for RF regression
#CVPredictionErrorbynodesize[,3,] gives results for CF prop. weighting
#CVPredictionErrorbynodesize[,4,] gives results for CF equal weighting

#These are AUC's for each nodesize using the best mtry from CV for each of 10 folds
#However, right now CV is done using LogLoss, not AUC. 
#Not used in paper
CVAUCbynodesize=array(NA, dim=c(nfolds,4,10))
for (fold in 1:nfolds){
  for(tech in 1:4)  { 
    CVAUCbynodesize[fold, tech,]=AUC[fold,tech,ParInds[fold,tech,][1],]
  }
}
#Dimensions are same as CVPredictionErrorbyNodesize

#These are mean leave probabilities for each nodesize using the best mtry combination from CV for each of 10 folds
MeanPredsbyNodesize=array(NA, dim=c(nfolds,4,10))
for (fold in 1:nfolds){
  for(tech in 2:5)  { 
    MeanPredsbyNodesize[fold, tech-1,]=rowMeans(TestPreds[,fold,tech,,ParInds[fold,tech-1,][1]])
  }
}
#Dimensions are same as CVPredictionErrorbyNodesize

#average across folds
CVPEN=apply(CVPredictionErrorbynodesize,c(2,3),mean)
CVAUCN=apply(CVAUCbynodesize,c(2,3),mean)
CVMeanPred=apply(MeanPredsbyNodesize, c(2,3), mean)


#Create dataframe containing loss function values for different aggregation and partitioning techniques
#and diferent nodesize values
ndsize=c(1,10,25,50,100,200,300,500,1000,5000)
#RF-B is RF(classification), RF-P is RF(regression), CF-WP is CF(proportional weighting), CF-P is CF(equal weighting)
Method=c(rep("RF-B",10),rep("RF-P",10),rep("CF-WP",10),rep("CF-P",10))
df=data.frame(rep(ndsize,4),Method, c(PE[1,],PE[2,],PE[3,],PE[4,]), c(bestAUC[1,],bestAUC[2,],bestAUC[3,],bestAUC[4,]),c(CVPEN[1,],CVPEN[2,],CVPEN[3,],CVPEN[4,]),c(CVAUCN[1,],CVAUCN[2,],CVAUCN[3,],CVAUCN[4,]),c(CVMeanPred[1,],CVMeanPred[2,],CVMeanPred[3,],CVMeanPred[4,])  )
names(df)=c("Nodesize", "Method", "BestError", "AUC", "NdSzError", "NdSzAUC", "NdMean")
#CV error is error for nodesize using optimal mtry, as determined in CV
#BestError is best obtained in test data for a given nodesize, optimizing over mtry and minbucket, regardless of whether this was suggested by CV

#Exclude RF-binary
df=df[df$Method!="RF-B",]

#save(file="CCDResults.Rdata")

#Find prediction error from test set correponding to what would have been chosen using cross validation
#Used in Table 5
CVPredictionError=matrix(NA, nrow=nfolds, ncol=4)
for (fold in 1:nfolds){
  for(tech in 1:4)  { 
    CVPredictionError[fold, tech]=PEAll[fold,tech,ParInds[fold,tech,][1],ParInds[fold,tech,][2]]
  }
}
