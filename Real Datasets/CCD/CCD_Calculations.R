#This is the file used to run the cross-validation. Rather than running all at once, 
#it was broken up and run differently for different nodesize values.
#See description file for details if seeking to replicate exactly. 

library(randomForest)
library(party)
library(plyr)
library(pROC)

source("CV_Predict_All_Funcs.R")

#Load in dataset
CCD=read.csv("CCD.csv")
#Load partitions for cross-validation
load("CCDCVpartitions.Rdata")

#set response variable name to "Class". Needed for functions in "CV_Predict_All_Funcs.R"
names(CCD)[25]="Class"
#Set response as 0-1 factor variable
CCD$Class=as.factor(as.character(CCD$Class))

#set number of reps and folds for simulation
DATA=CCD
nreps=1
nfolds=10

#set tuning parameters to use
mtry=c(4,8,16)  #mtry values checked
#simulation was split up and run on different ndsize values at a time. See description file if seeking to
#replicate exactly
#ndsize=c(1,10,25,50,100,200,300,500,1000,5000)
ndsize=c(1)  
mb=c(1/3)  #ratio of minbucket to minsplit. Kept at default of 1/3
ntrees=500 #number of trees

set.seed(1262017)
#Array to store OOB predictions for training cases
#Indices on OOBPreds and TestPreds are
#1-rep #2-fold #3-Pred Method #4-Case #5-mtry #6-nodesize #7-minbucket(only default used)
OOBPreds=array(NA, dim=c(nreps,nfolds, 5, nrow(DATA)-nrow(DATA)/nfolds, length(mtry), length(ndsize), length(mb)))
TestPreds=array(NA, dim=c(nreps,nfolds, 5, nrow(DATA)/nfolds, length(mtry), length(ndsize), length(mb)))
for(rep in 1:nreps){ #only did 1 rep
  for(fold in 1:nfolds){
    TRAIN=DATA[CVTRAINind[rep,fold,],]  #pull out training cases
    TEST=DATA[CVTESTind[rep,fold,],]    #pull out test cases
    #get predictions for all cases and all mtry, ndsize, mb values. 
    #note mtry ndsize, mb, are vectors and PredictionFunc will get predictions for all combinations of these
    Results=PredictionFunc(TRAIN,TEST, mtry, ndsize, mb, ntrees) 
    OOBPreds[rep,fold,,,,,]=Results[[1]]  #store OOB preds
    TestPreds[rep,fold,,,,,]=Results[[2]] #store test preds
   # print(c(paste0("rep=: ", rep),paste0("fold=: ", fold))) print when rep and fold complete
    save(OOBPreds, TestPreds, file="CCDRes1.Rdata")     
  }
}

