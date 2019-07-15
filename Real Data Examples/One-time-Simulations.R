library(partykit)
library(ggplot2)
library(stats)
#setwd("C:/Users/sagea-a/Desktop/Aggregation Paper")
setwd("~/OneDrive - Lawrence University/Research/Aggregation Paper")
setwd("C:/Users/sagea-a/Desktop/Aggregation Paper")
source("Aggregation_Functions.R")

########################################################################################################################

CCD <- read.csv("CCD.csv", header=TRUE)[,-1]
DATA <- CCD

set.seed(12072018)
samp <- sample(1:nrow(DATA), nrow(DATA)/2)
msvec <- c(20)
ms=1

TRAIN <- DATA[-samp,]
TEST <- DATA[samp,]
Res <- array(dim=c(length(msvec), 6, nrow(TEST))) #nodesize by prediction by test case
OOBRes <- array(dim=c(length(msvec), 6, nrow(TRAIN))) #nodesize by prediction by training case
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

save(Res, OOBRes, file="CCD1_m20.Rdata")


################################################################################################################################
#STEM dataset

STEM <- read.csv("STEM.csv", header=TRUE)[,-1]
STEM$LC_type <- as.numeric(STEM$LC_type != 0)  #Yes/No for LC


DATA <- STEM

set.seed(12072018)
samp <- sample(1:nrow(DATA), nrow(DATA)/2)
msvec <- c(20)
ms=1

TRAIN <- DATA[-samp,]
TEST <- DATA[samp,]
Res <- array(dim=c(length(msvec), 6, nrow(TEST))) #nodesize by prediction by test case
OOBRes <- array(dim=c(length(msvec), 6, nrow(TRAIN))) #nodesize by prediction by training case
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

save(Res, OOBRes, file="STEM_m20.Rdata")



## Get Boosting and LR Probability Estimates
#STEM dataset

load("STEM_m20.Rdata")
Probs <- Res[1,,]
dim(t(Probs))
Probs <-t(Probs)

STEM <- read.csv("STEM.csv", header=TRUE)[,-1]
STEM$LC_type <- as.numeric(STEM$LC_type != 0)  #Yes/No for LC
names(STEM)[31] <- "Y"

DATA <- STEM

set.seed(12072018)
samp <- sample(1:nrow(DATA), nrow(DATA)/2)
msvec <- c(20)
ms=1

TRAIN <- DATA[-samp,]
TEST <- DATA[samp,]

M <- glm(data=TRAIN, Y~., family="binomial")
LRPred <- c(predict(M, newdata=TEST, type="response"))

Probs <- data.frame(Probs)
Probs$LR <- LRPred




TRAINX <- as.matrix(TRAIN)[,-31]
TRAINY <- c(TRAIN$Y)
TESTX <- as.matrix(TRAIN)[,-31]
TESTY <- c(TRAIN$Y)



hyper_grid <- expand.grid(
  degree = 1:3, 
  nprune = seq(2, 100, length.out = 10) %>% floor()
)

tuned_mars <- train(
  x = subset(TRAIN, select = -Y),
  y = TRAIN$Y,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)

Marspred <- predict(tuned_mars, subset(TEST, select = -Y))

cor(Marspred, LRPred)


###
tune_grid <- expand.grid(
  nrounds = seq(from = 20, to = 200, by = 40),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 3, # with n folds 
  #index = createFolds(tr_treated$Id_clean), # fix the folds
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)

xgb_tune <- caret::train(
  x = subset(TRAIN, select = -Y), 
  y = TRAIN$Y, 
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE
)

xgb_tune
XGBPred <- predict(xgb_tune, newdata = subset(TEST, select = -Y))


Probs$BST <- XGBPred
Probs$MARS <- c(Marspred)
names(Probs) <- c(1:10)

ggpairs(Probs[,2:9])


AddLine <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_abline(slope=1, color="red", ...) 
  p
}

g <- ggpairs(Probs,columns = 2:9, lower = list(continuous = AddLine))
g

save(file="ProbsWorkspace.Rdata")

#########################################################################################################################
#Simple Simulation

msvec <- c(1)
ms=1

b0=-2.564
b1=1

set.seed(12072018)
size=2000 #Number of test and training cases
x1=rnorm(size,0,1)
x2=rnorm(size,0,1)
x3=rnorm(size,0,1)
x4=rnorm(size,0,1)
X=c(x1,x2,x3,x4)
dim(X) <- c(size, 4)
Y=c(rep(NA, size))
DATA=data.frame(X,Y)
eta=b0+b1*x1
prb=exp(eta)/(1+exp(eta))
DATA$Y=rbinom(length(prb),1,prb)


TRAIN <-DATA[1:1000,]
TEST <-DATA[1001:2000,]

Res <- array(dim=c(length(msvec), 6, nrow(TEST))) #nodesize by prediction by test case
OOBRes <- array(dim=c(length(msvec), 6, nrow(TRAIN))) #nodesize by prediction by training case
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

save(Res, OOBRes, prb, file="Sim1_m1.Rdata")

############################################################################################################################################
#simulation 2

set.seed(12072018)

msvec <- c(20)
ms <- 1
size <- 2000

X1<- matrix(rbinom(size*10, size=1, prob=0.5), nrow=size)
X2 <- rmvnorm(size, mean=rep(0, 20), sigma=diag(20))
X <- cbind(X1, X2)
Y <- c(rep(NA, size))
p1 <- X[,1]+10*abs(X[,11])+5*X[,2]+5*X[,3]
p2 <- X[,1]+10*abs(X[,12])+5*X[,4]
p3 <- X[,1]+10*abs(X[,13])
#p2 <- X[,2]+3*abs(X[,11]-X[,12])
#p3 <- X[,3]+abs(X[,13])
S <- p1+p2+p3
Probs <- data.frame(p1/S, p2/S, p3/S)
for (i in 1:size){
  Y[i]=sample(1:3, 1, prob=c(p1[i], p2[i], p3[i]))  
}
Y <- factor(Y)
summary(factor(Y))
DATA <- data.frame(X, Y)
TRAIN <-DATA[1:1000,]
TEST <-DATA[1001:2000,]


Res <- array(dim=c(length(msvec), 6, nrow(TEST),3)) #nodesize by prediction by test case by class
OOBRes <- array(dim=c(length(msvec), 6, nrow(TRAIN),3)) #nodesize by prediction by training case by class
  CNTR = ctree_control(teststat = "quad", testtype = "Univ", mincriterion = 0, saveinfo = FALSE, minsplit=msvec[ms], minbucket=ceiling(msvec[ms]/3))
  CF <- partykit::cforest(data=TRAIN, Y~., control=CNTR)
  PredSc <- predict(CF, newdata = TEST, scale=TRUE, type="prob")
  PredUnsc <- predict(CF, newdata = TEST, scale=FALSE, type="prob")
  nd <- predict(CF, newdata=TRAIN, scale=TRUE, type="node")  #which node do the training cases land in in each tree
  lw <- CF$weights #which training cases are used in each tree
  np <- vector(mode = "list", length = length(lw)) #vector for node predictions
  #calculate predictions for each test case
  PredDef <- sapply(1:nrow(TEST), DefPred3Class, Forest=CF, TEST=TEST, TRAIN=TRAIN, nd=nd, lw=lw, np=np) #number of trees predicting each class
  PredDef <- t(PredDef / sum(PredDef[,1]))
  OOBPreds <- sapply(1:nrow(TRAIN), GetOOBPred3Class, Forest=CF, TEST=TEST, TRAIN=TRAIN, nd=nd, lw=lw, np=np)  # predictions for each training case from each tree (tree x training case)
  OOBcases <- 1-matrix(unlist(lw), ncol=nrow(TRAIN), byrow=TRUE) # which cases are OOB (tree x training case)
  ERR <- t(apply(OOBPreds, 1, function(x){return((x!=TRAIN$Y))})) #was case misclassified (tree by training case)
  MCOOB <- OOBcases*ERR  #was a case both misclassified and OOB?
  TreeMC <- apply(MCOOB, 1, sum)/apply(OOBcases, 1, sum) #prop. or misclassified cases in each tree
  TreeProbs <- t(sapply(1:nrow(TEST), CalcTreeProbs, Forest=CF, TEST=TEST, TRAIN=TRAIN, nd=nd, lw=lw, np=np, TreeMC=TreeMC))
  PredOOBWt <- sapply(1:nrow(TEST), WtPredsOOBAcc, TreeMC=TreeMC, lw=lw, TreeProbs=TreeProbs)
  GiniWts <- sapply(1:nrow(TEST), WtPredsGiniPurity , Forest=CF, TRAIN=TRAIN, TEST=TEST, nd=nd, lw=lw, np=np) #Weights for each tree
  PredVarWt <- sapply(1:nrow(TEST), WtPredsGini ,  lw=lw, GiniWts=GiniWts, TreeProbs=TreeProbs) 
  #Calculate OOB Predictions Using Each Approach
  PredScOOB <- predict(CF, OOB=TRUE, scale=TRUE, type="prob")
  PredUnscOOB <- predict(CF, OOB=TRUE, scale=FALSE, type="prob")
  PredDefOOB <- sapply(1:nrow(TRAIN), DefPredOOB3Class, Forest=CF, TEST=TEST, TRAIN=TRAIN, nd=nd, lw=lw, np=np, OOBcases=OOBcases) 
  PredDefOOB <- apply(PredDefOOB , 1, function(x) {x/ apply(PredDefOOB, 2, sum)})
  GiniWtsOOB <- sapply(1:nrow(TRAIN), WtPredsGiniPurityOOB , Forest=CF, TRAIN=TRAIN, nd=nd, lw=lw, np=np, OOBcases=OOBcases) #Weights for each tree
  PredVarOOB <- sapply(1:nrow(TRAIN), WtPredsGini ,  lw=lw, GiniWts=GiniWtsOOB, TreeProbs=TreeProbs) 
  Res[ms, 1, ,] <- TEST$Y
  Res[ms, 2, ,] <- PredSc
  Res[ms, 3, ,] <- PredUnsc
  Res[ms, 4, ,] <- PredDef
  Res[ms, 5, ,] <- t(PredVarWt)
  Res[ms, 6, ,] <- t(PredOOBWt)
  
  OOBRes[ms, 1, ,] <- TRAIN$Y
  OOBRes[ms, 2, ,] <- PredScOOB
  OOBRes[ms, 3, ,] <- PredUnscOOB
  OOBRes[ms, 4, ,] <- PredDefOOB
  OOBRes[ms, 5, ,] <- t(PredVarOOB)
  OOBRes[ms, 6, ,] <- PredScOOB #can't evaluate OOBwt method on OOB preds. Use regular scaled predictions. 
  
  save(Res, OOBRes, Probs, file="Sim2_m20.Rdata")
  
X <- Probs[1001:2000,]  
Y <- Res[1, 2, ,]

cor(X[,1], Y[,1])
cor(X[,2], Y[,2])
cor(X[,3], Y[,3])



############################################################################################################

#Regression Example

library(AmesHousing)
Ames <- make_ames()
names(Ames)[names(Ames)=="Sale_Price"] <- "Y"
msvec <- c(20)
ms=1
set.seed(12172018)
samp <- sample(1:nrow(Ames))

TRAIN <- Ames[samp[1:1465], ]
TEST <- Ames[samp[1466:2930],]


Res <- array(dim=c(length(msvec), 6, nrow(TEST))) #nodesize by prediction by test case
OOBRes <- array(dim=c(length(msvec), 6, nrow(TRAIN))) #nodesize by prediction by training case
CNTR = ctree_control(teststat = "quad", testtype = "Univ", mincriterion = 0, saveinfo = FALSE, minsplit=msvec[ms], minbucket=ceiling(msvec[ms]/3))

#treat response as categorical
CF <- partykit::cforest(data=TRAIN, Y~., control=CNTR)

PredSc <- predict(CF, newdata = TEST, scale=TRUE)
PredUnsc <- predict(CF, newdata = TEST, scale=FALSE)
nd <- predict(CF, newdata=TRAIN, scale=TRUE, type="node")  #which node do the training cases land in in each tree
lw <- CF$weights #which training cases are used in each tree
np <- vector(mode = "list", length = length(lw)) #vector for node predictions
#calculate predictions for each test case
PredVar <- sapply(1:nrow(TEST), WtbyVarReg, Forest=CF, TEST=TEST, TRAIN=TRAIN, nd=nd, lw=lw, np=np) #predictions when trees are weighted by variance in terminal node
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

save(Res, OOBRes, prb, file="AmesHous_m20.Rdata")

##################################################################################
#Random survival forests
library(TH.data) 
library(survival)

data("GBSG2", package = "TH.data")
samp <- sample(1:686, 343)
TRAIN <- GBSG2[-samp, ]
TEST <- GBSG2[1, ]


bst <- cforest(Surv(time, cens) ~ ., data = TRAIN, ntree = 500)

### estimate conditional Kaplan-Meier curves
Preds1 <- predict(bst, newdata = TEST, OOB = TRUE, type = "prob", scale=TRUE)
Preds2 <- predict(bst, newdata = TEST, OOB = TRUE, type = "prob", scale=FALSE)

df <- data.frame(Preds1, Preds2)
df1 <- df[which(df$Preds1!=Inf), ]

#at time 7
#6668 at risk
#5 events
#0 censored

#at end
#3 at risk
#0 events
#3 censored at last point
#total 3481 events, 3243 censored, of orig. 6724
# survival prob of 3481/6724


