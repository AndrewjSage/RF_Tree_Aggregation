setwd("/work/STAT/ajsage")
source("Aggregation_Functionsb.R")

library(partykit)

#setwd("C:/Users/sagea-a/Desktop/Aggregation Paper")
#setwd("~/OneDrive - Lawrence University/Research/Aggregation Paper")

CCD <- read.csv("CCD.csv", header=TRUE)[,-1]

#######################################################################################################
STEM <- read.csv("STEM.csv", header=TRUE)[,-1]
STEM$LC_type <- as.numeric(STEM$LC_type != 0)  #Yes/No for LC
DATA <- STEM
msvec <- c(1, 20)

samp <- sample(1:nrow(DATA))

TRAIN <- DATA[-samp,]
TEST <- DATA[samp,]
Res <- array(dim=c(length(msvec), 6, nrow(TEST))) #nodesize by prediction by test case
OOBRes <- array(dim=c(length(msvec), 6, nrow(TRAIN))) #nodesize by prediction by training case
for (ms in 1:length(msvec)){
  CNTR = ctree_control(teststat = "quad", testtype = "Univ", mincriterion = 0, saveinfo = FALSE, minsplit=msvec[ms], minbucket=ceiling(msvec[ms]/3))
  #treat response as categorical
  CF <- cforest(data=TRAIN, factor(Y)~., control=CNTR)
  PredSc <- predict(CF, newdata = TEST, scale=TRUE, type="prob")
  PredUnsc <- predict(CF, newdata = TEST, scale=FALSE, type="prob")
  nd <- predict(CF, newdata=TRAIN, scale=TRUE, type="node")  #which node do the training cases land in in each tree
  lw <- CF$weights #which training cases are used in each tree
  np <- vector(mode = "list", length = length(lw)) #vector for node predictions
  #calculate predictions for each test case
  PredDef <- sapply(1:nrow(TEST), DefPred2Class, Forest=CF, TEST=TEST, TRAIN=TRAIN, nd=nd, lw=lw, np=np)
  PredVar <- sapply(1:nrow(TEST), WtbyVar2Class, Forest=CF, TEST=TEST, TRAIN=TRAIN, nd=nd, lw=lw, np=np) #predictions when trees are weighted by variance in terminal node
  OOBPreds <- sapply(1:nrow(TRAIN), GetOOBPred, Forest=CF, TEST=TEST, TRAIN=TRAIN, nd=nd, lw=lw, np=np)  # predictions for each training case from each tree (tree x training case)
  OOBcases <- 1-matrix(unlist(lw), ncol=nrow(TRAIN), byrow=TRUE) # which cases are OOB (tree x training case)
  ERR <- t(apply(OOBPreds, 1, function(x){return((x!=TRAIN$Y))})) #was case misclassified? (tree by training case)
  MCOOB <- OOBcases*ERR  #was a case both misclassified and OOB?
  TreeMC <- apply(MCOOB, 1, sum)/apply(OOBcases, 1, sum) #prop. or misclassified cases in each tree
  PredOOBWt <- sapply(1:nrow(TEST), WtbyOOBPerf, Forest=CF, TEST=TEST, TRAIN=TRAIN, nd=nd, lw=lw, np=np, TreeMC=TreeMC)
  
  #Calculate OOB Predictions Using Each Approach
  PredScOOB <- predict(CF, OOB=TRUE, scale=TRUE, type="prob")
  PredUnscOOB <- predict(CF, OOB=TRUE, scale=FALSE, type="prob")
  PredDefOOB <- sapply(1:nrow(TRAIN), DefPredOOB2Class, Forest=CF, TEST=TEST, TRAIN=TRAIN, nd=nd, lw=lw, np=np, OOBcases=OOBcases) 
  PredVarOOB <- sapply(1:nrow(TRAIN), WtbyVarOOB2Class, Forest=CF, TEST=TEST, TRAIN=TRAIN, nd=nd, lw=lw, np=np, OOBcases=OOBcases)
  
  Res[ms, 1, ] <- TEST$Y
  Res[ms, 2, ] <- PredSc[,2]
  Res[ms, 3, ] <- PredUnsc[,2]
  Res[ms, 4, ] <- PredDef
  Res[ms, 5, ] <- PredVar
  Res[ms, 6, ] <- PredOOBWt
  
  OOBRes[ms, 1, ] <- TRAIN$Y
  OOBRes[ms, 2, ] <- PredScOOB[,2]
  OOBRes[ms, 3, ] <- PredUnscOOB[,2]
  OOBRes[ms, 4, ] <- PredDefOOB
  OOBRes[ms, 5, ] <- PredVarOOB
  OOBRes[ms, 6, ] <- PredScOOB[,2] #can't evaluate OOBwt method on OOB preds. Use regular scaled predictions. 
  
}

save(Res, OOBRes, filename="STEM1Run.Rdata")
#########################################################################################################
CCD <- read.csv("CCD.csv", header=TRUE)[,-1]

DATA <- CCD
msvec <- c(1, 20)

samp <- sample(1:nrow(DATA))

TRAIN <- DATA[-samp,]
TEST <- DATA[samp,]
Res <- array(dim=c(length(msvec), 6, nrow(TEST))) #nodesize by prediction by test case
OOBRes <- array(dim=c(length(msvec), 6, nrow(TRAIN))) #nodesize by prediction by training case
for (ms in 1:length(msvec)){
  CNTR = ctree_control(teststat = "quad", testtype = "Univ", mincriterion = 0, saveinfo = FALSE, minsplit=msvec[ms], minbucket=ceiling(msvec[ms]/3))
  #treat response as categorical
  CF <- cforest(data=TRAIN, factor(Y)~., control=CNTR)
  PredSc <- predict(CF, newdata = TEST, scale=TRUE, type="prob")
  PredUnsc <- predict(CF, newdata = TEST, scale=FALSE, type="prob")
  nd <- predict(CF, newdata=TRAIN, scale=TRUE, type="node")  #which node do the training cases land in in each tree
  lw <- CF$weights #which training cases are used in each tree
  np <- vector(mode = "list", length = length(lw)) #vector for node predictions
  #calculate predictions for each test case
  PredDef <- sapply(1:nrow(TEST), DefPred2Class, Forest=CF, TEST=TEST, TRAIN=TRAIN, nd=nd, lw=lw, np=np)
  PredVar <- sapply(1:nrow(TEST), WtbyVar2Class, Forest=CF, TEST=TEST, TRAIN=TRAIN, nd=nd, lw=lw, np=np) #predictions when trees are weighted by variance in terminal node
  OOBPreds <- sapply(1:nrow(TRAIN), GetOOBPred, Forest=CF, TEST=TEST, TRAIN=TRAIN, nd=nd, lw=lw, np=np)  # predictions for each training case from each tree (tree x training case)
  OOBcases <- 1-matrix(unlist(lw), ncol=nrow(TRAIN), byrow=TRUE) # which cases are OOB (tree x training case)
  ERR <- t(apply(OOBPreds, 1, function(x){return((x!=TRAIN$Y))})) #was case misclassified? (tree by training case)
  MCOOB <- OOBcases*ERR  #was a case both misclassified and OOB?
  TreeMC <- apply(MCOOB, 1, sum)/apply(OOBcases, 1, sum) #prop. or misclassified cases in each tree
  PredOOBWt <- sapply(1:nrow(TEST), WtbyOOBPerf, Forest=CF, TEST=TEST, TRAIN=TRAIN, nd=nd, lw=lw, np=np, TreeMC=TreeMC)
  
  #Calculate OOB Predictions Using Each Approach
  PredScOOB <- predict(CF, OOB=TRUE, scale=TRUE, type="prob")
  PredUnscOOB <- predict(CF, OOB=TRUE, scale=FALSE, type="prob")
  PredDefOOB <- sapply(1:nrow(TRAIN), DefPredOOB2Class, Forest=CF, TEST=TEST, TRAIN=TRAIN, nd=nd, lw=lw, np=np, OOBcases=OOBcases) 
  PredVarOOB <- sapply(1:nrow(TRAIN), WtbyVarOOB2Class, Forest=CF, TEST=TEST, TRAIN=TRAIN, nd=nd, lw=lw, np=np, OOBcases=OOBcases)
  
  Res[ms, 1, ] <- TEST$Y
  Res[ms, 2, ] <- PredSc[,2]
  Res[ms, 3, ] <- PredUnsc[,2]
  Res[ms, 4, ] <- PredDef
  Res[ms, 5, ] <- PredVar
  Res[ms, 6, ] <- PredOOBWt
  
  OOBRes[ms, 1, ] <- TRAIN$Y
  OOBRes[ms, 2, ] <- PredScOOB[,2]
  OOBRes[ms, 3, ] <- PredUnscOOB[,2]
  OOBRes[ms, 4, ] <- PredDefOOB
  OOBRes[ms, 5, ] <- PredVarOOB
  OOBRes[ms, 6, ] <- PredScOOB[,2] #can't evaluate OOBwt method on OOB preds. Use regular scaled predictions. 
  
}

save(Res, OOBRes, filename="CCD1Run.Rdata")

