#This script reads in the predictions and produces summary information used 
#to create the graphics in the paper.

#Indices on OOBPreds and TestPreds are
#1-rep #2-fold #3-Pred Method #4-Case #5-mtry #6-nodesize #7-minbucket
#Here we leave minbucket at its default of 1/3*minsplit

###################################################################################################
#Different loss functions. We calculate all of them, even though only use LogLoss in paper

SumLogLik=function(phat, Y){
  VAL=NA
  VAL=log(phat)*Y+log(1-phat)*(1-Y)  #First term penalizes cases where Y=1 by -log(P(Y=1)) (The lower the probability, the worse the penalty since log punishes low numbers the most). Second term punishes cases where Y=0, by 1-P(Y=1). So higher probablity of Y=1 gets penalized more when Y is actually 0. 
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
#######################################################################################################

#Find best parameters for each rep and fold
FindBest=function(nreps, nfolds, OOBError){ #For loss func, 1=Log, 2=MSE, 3=Sph,4=Boosting Loss, 5=AUC #should usually use log
  Inds=array(NA, dim=c(nreps, nfolds, 4,3))
  for (rep in 1:nreps){
    for (fold in 1:nfolds){
      for(tech in 1:4)  { #for logloss(for others, change 1 in third index)
        Inds[rep,fold,tech,] = which(OOBError[rep,fold,1,tech,,,] == min(OOBError[rep,fold,1,tech,,,]), arr.ind=TRUE)[1,] #1 in 3rd index is for log loss, [1,]pulls off first in case of tie
      }
    }
  }
  return(Inds)
}


#Computes error for each of the above loss functions for each fold, mtry, and nodesize comb.
#Can be run on OOB or predicted probs
ComputeError=function(TestPreds){
PredError=array(NA, dim=c(dim(TestPreds)[1],dim(TestPreds)[2], 4,3,dim(TestPreds)[5],dim(TestPreds)[6],dim(TestPreds)[7] ))
for(rep in 1:dim(TestPreds)[1]){
  for(fold in 1:dim(TestPreds)[2]){
    for (mtry in 1:dim(TestPreds)[5]){
      for (ndsize in 1:dim(TestPreds)[6]){
        for (minb in 1:dim(TestPreds)[7]){
    Predictions=TestPreds[rep,fold,,,mtry,ndsize,minb]
    PredError[rep,fold,1,1,mtry,ndsize,minb]=SumLogLik(Predictions[2,], Predictions[1,]) #RF
    PredError[rep,fold,2,1,mtry,ndsize,minb]=SumLogLik(Predictions[3,], Predictions[1,]) #RFr
    PredError[rep,fold,3,1,mtry,ndsize,minb]=SumLogLik(Predictions[4,], Predictions[1,]) #CF
    PredError[rep,fold,4,1,mtry,ndsize,minb]=SumLogLik(Predictions[5,], Predictions[1,]) #CFa
    PredError[rep,fold,1,2,mtry,ndsize,minb]=SSE(Predictions[2,], Predictions[1,]) #RF
    PredError[rep,fold,2,2,mtry,ndsize,minb]=SSE(Predictions[3,], Predictions[1,]) #RFr
    PredError[rep,fold,3,2,mtry,ndsize,minb]=SSE(Predictions[4,], Predictions[1,]) #CF
    PredError[rep,fold,4,2,mtry,ndsize,minb]=SSE(Predictions[5,], Predictions[1,]) #CFa
    PredError[rep,fold,1,3,mtry,ndsize,minb]=BoostingLoss(Predictions[2,], Predictions[1,])
    PredError[rep,fold,2,3,mtry,ndsize,minb]=BoostingLoss(Predictions[3,], Predictions[1,])
    PredError[rep,fold,3,3,mtry,ndsize,minb]=BoostingLoss(Predictions[4,], Predictions[1,])
    PredError[rep,fold,4,3,mtry,ndsize,minb]=BoostingLoss(Predictions[5,], Predictions[1,])
        }}}
    print(fold)
    }}
return(PredError)
}


#Computes AUC for each of the above loss functions for each fold, mtry, and nodesize comb.
#Can be run on OOB or predicted probs
library(pROC)
ComputeAUC=function(TestPreds){
  AUC=array(NA, dim=c(dim(TestPreds)[1],dim(TestPreds)[2], 4,dim(TestPreds)[5],dim(TestPreds)[6],dim(TestPreds)[7] ))
  for(rep in 1:dim(TestPreds)[1]){
    for(fold in 1:dim(TestPreds)[2]){
      for (mtry in 1:dim(TestPreds)[5]){
        for (ndsize in 1:dim(TestPreds)[6]){
          for (minb in 1:dim(TestPreds)[7]){
            Predictions=TestPreds[rep,fold,,,mtry,ndsize,minb]
           AUC[rep,fold,1,mtry,ndsize,minb]=auc(Predictions[1,], Predictions[2,]) #RFclassification (not used in paper)
            AUC[rep,fold,2,mtry,ndsize,minb]=auc(Predictions[1,], Predictions[3,]) #RF regression
            AUC[rep,fold,3,mtry,ndsize,minb]=auc(Predictions[1,], Predictions[4,]) #CF proportional weighting
            AUC[rep,fold,4,mtry,ndsize,minb]=auc(Predictions[1,], Predictions[5,]) #CF equal weighting
          }}}
      print(fold)
      }}
  return(AUC)
}

#Combine Predictions from 2 different large datafiles
library(abind)
load("Predictions1.Rdata")
Preds1=Predictions
load("Predictions2.Rdata")
Preds2=Predictions
TestPredictions=abind(Preds1, Preds2, along=6)
save(TestPredictions, file="CombinedTestPredictions.Rdata")

TestPreds=abind(Preds1, Preds2, along=6)
TestPreds[TestPreds>1]=0.00001  # fix for infinities. This problem only occurs for RF-classification, which wasn't used in paper

PredError=ComputeError(TestPreds)
save(PredError, file="PredError.Rdata")
AUC=ComputeAUC(TestPreds)
save(AUC, file="AUC.Rdata")


load("Predictions1.Rdata")
OOBPreds1=OOBPredictions
load("Predictions2.Rdata")
OOBPreds2=OOBPredictions

OOBError1=ComputeError(OOBPreds1)
OOBPreds2[OOBPreds2>1]=0.00001  # fix for infinities. This problem only occurs for RF-classification, which wasn't used in paper
OOBError2=ComputeError(OOBPreds2)
OOBError=abind(OOBError1, OOBError2, along=6)
save(OOBError, file="OOBErrors.Rdata")



###########################################################
setwd("~/Box Sync/Iowa State/Research/CF Aggregation Paper/Code for Calibration Paper/Current Code/CV_Predict_STEM/Datafiles")
load("OOBErrors.Rdata")
load("AUC.Rdata")
load("PredError.Rdata")
load("CombinedTestPredictions.Rdata")

#Dimensions of PredError and OOBError are
#1-rep 2-fold 3-method 4-case 5-mtry 6-nodesize 7-minb

TestPredictions[TestPredictions>1]=0.00001  # fix for infinities. This problem only occurs for RF-classification, which wasn't used in paper
TestPreds=TestPredictions
TestPreds=TestPreds[1,,,,,,] #Only did 1 rep


#For each method and find best choice of mtry, nodesize using OOB cases
#We exclusively use logloss from now on. Change third index as described below to use other loss function
FindBest=function(nfolds, OOBError){ #For loss func, 1=Log, 2=MSE, 3=Sph,4=Boosting Loss, 5=AUC #should usually use log
  Inds=array(NA, dim=c( nfolds, 4,2))  #Last index is now 2 since we're only concerned with mtry and minsplit
  for (fold in 1:nfolds){
    for(tech in 1:4)  { #for logloss(for others, change 1 in third index)
      Inds[fold,tech,] = which(OOBError[fold,tech,1,,,3] == min(OOBError[fold,tech,1,,,3]), arr.ind=TRUE)[1,] #1 in 3rd index is for log loss, [1,]pulls off first in case of tie  #3 in last spot is since we're keeping mb at its default
    }
  }
  return(Inds)
}

#ParInds contain indices of best mtry, nodesize combination for each of 10 folds.
OOBErrors=OOBError[1,,,,,,]
ParInds=FindBest(9,OOBErrors)


#Get rid of first dim since there is only 1 rep
PEAll=PredError[1,,,,,,]
#Focus on LogLoss and default mb
PEAll=PEAll[,,1,,,3]

#Get preds resulting from best mtry 
PE=apply(PEAll,c(1,2,4),min)

#Average across folds
PE=apply(PE,c(2,3),mean)

#Reduce by 1 dim since only 1 rep and also fix minbucket to default
AUC1=AUC[1,,,,,3]
#Get best AUCs as a function of nodesize)
bestAUC=apply(AUC1,c(1,2,4),min)
#Average across folds
bestAUC=apply(bestAUC,c(2,3),mean)


nfolds=9

#These are logloss for each nodesize using the best mtry from CV for each of 9 folds
CVPredictionErrorbynodesize=array(NA, dim=c(nfolds,4,16))
for (fold in 1:nfolds){
  for(tech in 1:4)  { 
    for(ndsize in 1:16)
      CVPredictionErrorbynodesize[fold, tech, ]=PEAll[fold,tech,ParInds[fold,tech,][1],]
  }
}

#These are AUC's for each nodesize using the best mtry from CV for each of 9 folds
CVAUCbynodesize=array(NA, dim=c(nfolds,4,16))
for (fold in 1:nfolds){
  for(tech in 1:4)  { 
    CVAUCbynodesize[fold, tech,]=AUC1[fold,tech,ParInds[fold,tech,][1],]
  }
}

#These are leave probabilities for each nodesize using the best mtry from CV for each of 9 folds
MeanPredsbyNodesize=array(NA, dim=c(nfolds,4,16))  #nfolds x number of techniques x nodesize
for (fold in 1:nfolds){
  for(tech in 2:5)  { 
    MeanPredsbyNodesize[fold, tech-1,]=colMeans(TestPreds[fold,tech,,ParInds[fold,tech-1,][1],,3]) #Last index fixed at 3 using default mb
  }
}

#average across folds
CVPEN=apply(CVPredictionErrorbynodesize,c(2,3),mean)
CVAUCN=apply(CVAUCbynodesize,c(2,3),mean)
CVMeanPred=apply(MeanPredsbyNodesize, c(2,3), mean)

#Create dataframe containing loss function values for different aggregation and partitioning techniques
#and diferent nodesize values
ndsize=c(1,5,10,25,50,75,100,150,200,250,300,400,500, 1000, 2000, 3000)
LogLossCF=PE[3,]
LogLossCFA=PE[4,]
Method=c(rep("RF-TV",16),rep("RF-TP",16),rep("CF-WTP",16),rep("CF-TP",16))
df=data.frame(rep(ndsize,4),Method, c(PE[1,],PE[2,],PE[3,],PE[4,]), c(bestAUC[1,],bestAUC[2,],bestAUC[3,],bestAUC[4,]),c(CVPEN[1,],CVPEN[2,],CVPEN[3,],CVPEN[4,]),c(CVAUCN[1,],CVAUCN[2,],CVAUCN[3,],CVAUCN[4,]),c(CVMeanPred[1,],CVMeanPred[2,],CVMeanPred[3,],CVMeanPred[4,])  )
names(df)=c("Nodesize", "Method", "BestError", "AUC", "NdSzError", "NdSzAUC", "NdMean")
#CV error is error for nodesize using optimal mtry, minbucket, as determined in CV
#BestError is best obtained in test data for a given nodesize, optimizing over mtry and minbucket, regardless of whether this was suggested by CV
#Same for AUC
#qplot(df$Nodesize, df$Error, colour=df$Method, xlim=c(0,3000), ylim=c(500,700))

#Exclude RF-classification
df=df[df$Method!="RF-TV",]

#save(file="STEMResults.Rdata")

#Find prediction error from test set correponding to what would have been chosen using cross validation
#Used in Table 5CVPredictionError=matrix(NA, nrow=nfolds, ncol=4)
for (fold in 1:nfolds){
  for(tech in 1:4)  { 
    CVPredictionError[fold, tech]=PEAll[fold,tech,ParInds[fold,tech,][1],ParInds[fold,tech,][2]]
  }
}
