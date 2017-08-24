#This is the file used to run the cross-validation. Rather than running all at once, 
#it was broken up and run differently for different nodesize values.
#See description file for details if seeking to replicate exactly. 

library(randomForest)
library(party)
library(plyr)
library(pROC)

source("~/Core Aggregation Functions/CV_Predict_All_Funcs.R")
#Load in dataset
STEM=readRDS("STEM.rds")
#Load partitions for cross-validation
load("STEMCVpartitions_Init.Rdata")

#set number of reps and folds for simulation
DATA=STEM
nreps=1
nfolds=9

#set tuning parameters to use
mtry=c(3,5,10,20)  #mtry values checked
#simulation was split up and run on different ndsize values at a time. See description file if seeking to
#replicate exactly
#ndsize=c(1,5,10,25,50,75,100,150,200,250,300,400,500, 1000, 2000, 3000)
ndsize=c(1)
mb=c(0, 1/6, 1/3, 2/3, 1) #ratio of minbucket to minsplit. Kept at default of 1/3
ntrees=500  #number of trees

set.seed(1262017)
#Array to store OOB predictions for training cases
#Indices on OOBPreds and TestPreds are
#1-rep #2-fold #3-Pred Method #4-Case #5-mtry #6-nodesize #7-minbucket(only default used)
OOBPreds=array(NA, dim=c(nreps,nfolds, 5, nrow(DATA)-nrow(DATA)/nfolds, length(mtry), length(ndsize), length(mb)))
TestPreds=array(NA, dim=c(nreps,nfolds, 5, nrow(DATA)/nfolds, length(mtry), length(ndsize), length(mb)))
#Dimensions
#1=rep 2=fold 3=LossFunc 4=RFTech 5=mtry 6=nodesize 7=minbucket
for(rep in 1:nreps){ #only did 1 rep
  for(fold in 1:nfolds){
    TRAIN=DATA[CVTRAINind[rep,fold,],]  #pull out training cases
    TEST=DATA[CVTESTind[rep,fold,],]   #pull out test cases
    #get predictions for all cases and all mtry, ndsize, mb values. 
    #note mtry ndsize, mb, are vectors and PredictionFunc will get predictions for all combinations of these
    Results=PredictionFunc(TRAIN,TEST, mtry, ndsize, mb, ntrees)
    OOBPreds[rep,fold,,,,,]=Results[[1]]  #store OOB preds
    TestPreds[rep,fold,,,,,]=Results[[2]] #store test preds
    #print(c(paste0("rep=: ", rep),paste0("fold=: ", fold)))
    save(OOBPreds, TestPreds, file="STEMRes_Init1.Rdata")  
  }
}

