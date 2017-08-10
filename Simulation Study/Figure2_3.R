#Simulation study used to examine calibration of proportional and equally weighted estimates
#This takes a long time to run!

library(randomForest)  #this was used in the simulation although not discussed in the paper
library(party)
library(plyr)

#Selected to hit target estimated success probabilities of 0.05, 0.1, 0.2, 0.3, 04, 0.5
b=c(-3.371,-2.564,-1.65,-1.018,-.490,0)
b1=1

#generate predictors
set.seed(02172016)
size=20000 #Total number of cases simulated, hald training, half test
x1=rnorm(size,0,1)
x2=rnorm(size,0,1)
x3=rnorm(size,0,1)
x4=rnorm(size,0,1)
X=c(x1,x2,x3,x4)
dim(X) <- c(size, 4)
Class=c(rep(NA, size))
DATA=data.frame(X,Class)
trainprop=.5  #half of cases will be training cases, others test
ntrees=500

#Setup matrices to store estimated probabilities for each case  
RespSummary=matrix(NA, nrow=nrow(DATA), ncol=length(b))  #stores true 0-1 responses for both test and training data
#dimensions are ntest by 6 (cols represent different balance/imbalance rations)
RFPrbMat=matrix(NA, nrow=nrow(DATA)*(1-trainprop), ncol=length(b)) #stores RF estimates for test data
CFPrbMat=matrix(NA, nrow=nrow(DATA)*(1-trainprop), ncol=length(b)) #stores CF prop. weighted estimates for test data
CFPrb_uwMat=matrix(NA, nrow=nrow(DATA)*(1-trainprop), ncol=length(b)) #stores CF equally weighted estimates for test data
TruePrbMat=matrix(NA, nrow=nrow(DATA)*(1-trainprop), ncol=length(b)) #stores true success probabilities from model

#Simulate responses
for (ind in 1:length(b)){
  b0=b[ind]
  eta=b0+b1*x1
  for(i in 1:size){
    DATA$Class[i]=rbinom(1,1,exp(eta[i])/(1+exp(eta[i])))
  }
  #Store response as 0-1 factor variable
  DATA$Class=as.factor(DATA$Class)
  
  #Split into test, training data
  TRAIN=DATA[1:(size*trainprop), ]
  TEST=DATA[(size*trainprop+1):size, ]
  #Grow random forest
  #Using default setting
  CF=cforest(Class~., data=TRAIN, controls=cforest_unbiased(ntree=ntrees, mtry=2, minsplit=20, minbucket=7))
  #Using full splitting
  #CF=cforest(Class~., data=TRAIN, controls=cforest_unbiased(ntree=ntrees, mtry=2, minsplit=1, minbucket=1))
  CFPred=unlist(predict(CF, newdata=TEST, type="prob")) 
  CFPrb=CFPred[seq(2,2*nrow(TEST),2)] #proportionally weighted cForest estimates
  source("~/Aggregation Function/CF_Weighting_Func.R")  
  CFPrb_uw=UnweightCF(CF,TRAIN,TEST,ntrees)[,2]  #equally weighted estimates
  RF=randomForest(Class~., data=TRAIN, ntree=ntrees)  #RF not actually used in paper
  RFPrb <- predict(RF, newdata=TEST, type="prob")[,2]
  RespSummary[,ind]=DATA$Class
  LinPred=b0+b1*TEST$X1   #Linear predictor
  TruePrbMat[,ind]=exp(LinPred)/(1+exp(LinPred))  #Store true success probabilities for given index
  RFPrbMat[,ind]=RFPrb  #store RF estimates for given index
  CFPrbMat[,ind]=CFPrb  #store CF prop. weighted estimates for a given index
  CFPrb_uwMat[,ind]=CFPrb_uw #store CF equally weighted estimates for a given index
}

#save(RespSummary, RFPrbMat, CFPrbMat, CFPrb_uwMat,TruePrbMat,TRAIN, TEST, file="ArtData2_defaultMinbucket.Rdata")
#save(RespSummary, RFPrbMat, CFPrbMat, CFPrb_uwMat,TruePrbMat,TRAIN, TEST, file="ArtData.Rdata")
#ArtData.Rdata gives summary for minbucket=minsplit=1
#ArtData2.Rdata gives summary for default parameters minsplit=20, minbucket=7

###################################################################################################
#Code to create graphs for paper
load("ArtData2_defaultMinbucket.Rdata")   #For simulation results with default settings
load("ArtData.Rdata")   #Simulation results with small nodesizes

postscript("Smallnode_BP.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 9, width = 7)
tiff("Def_BP.tiff", width = 7, height = 9, units = 'in', res = 1200)
par(mfrow=c(3,2))
par(mar = rep(2, 4))
i=1
boxplot(TruePrbMat[,i],CFPrb_uwMat[,i],CFPrbMat[,i], outline=F,range=1,  names=c("True", "EW", "PW"), main="p=.05")
#p=.10
i=2
boxplot(TruePrbMat[,i],CFPrb_uwMat[,i],CFPrbMat[,i], outline=F,range=1,  names=c("True", "EW", "PW"), main="p=.10")
#p=.20
i=3
boxplot(TruePrbMat[,i],CFPrb_uwMat[,i],CFPrbMat[,i], outline=F,range=1,  names=c("True", "EW", "PW"), main="p=.20")
#p=.30
i=4
boxplot(TruePrbMat[,i],CFPrb_uwMat[,i],CFPrbMat[,i], outline=F,range=1,  names=c("True", "EW", "PW"), main="p=.30")
#p=.40
i=5
boxplot(TruePrbMat[,i],CFPrb_uwMat[,i],CFPrbMat[,i], outline=F,range=1,  names=c("True", "EW", "PW"), main="p=.40")
#p=.50
i=6
boxplot(TruePrbMat[,i],CFPrb_uwMat[,i],CFPrbMat[,i], outline=F,range=1,  names=c("True", "EW", "PW"), main="p=.50")
dev.off()

#####################################################################
