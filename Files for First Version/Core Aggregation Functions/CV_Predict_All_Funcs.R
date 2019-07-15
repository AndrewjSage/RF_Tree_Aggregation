library(randomForest)
library(party)
library(plyr)
library(pROC)

source("CF_Weighting_Func.R")

#Takes in 1 dataset and makes predictions on OOB cases to determine best technique to use
PredictionFunc=function(TRAIN,TEST, mtry, ndsize, mb, ntrees){
  OOBPreds=array(NA, dim=c(5, nrow(TRAIN), length(mtry), length(ndsize), length(mb)))
  TestPreds=array(NA, dim=c(5, nrow(TEST), length(mtry), length(ndsize), length(mb)))
  for(m in 1:length(mtry)){
    for(n in 1:length(ndsize)){
      RF=randomForest(Class~., data=TRAIN, ntree=ntrees, nodesize=ndsize[n], mtry=mtry[m])  #RFPrb
      Prb=RF$votes[,2] 
      Prb[abs(Prb)<10^-5]=min(Prb[abs(Prb)>10^-5])/2   #Deal with 0 probabilities
      Prb[abs(Prb-1)<10^-5]=(max(Prb[abs(Prb-1)>10^-5])+1)/2  #Deal with 1 probabilities
      OOBPreds[2,,m,n,1:length(mb)] <- Prb  #Note all entries for minbucket same since this isn't par. in RF
      #now predict test cases
      RFPrb <- predict(RF, newdata=TEST, type="prob")[,2]
      RFPrb[abs(RFPrb)<10^-5]=min(RFPrb[abs(RFPrb)>10^-5])/2   #Deal with 0 probabilities
      RFPrb[abs(RFPrb-1)<10^-5]=(max(RFPrb[abs(RFPrb-1)>10^-5])+1)/2  #Deal with 1 probabilities
      TestPreds[2,,m,n,1:length(mb)] <- RFPrb
      RFr=randomForest(as.numeric(as.character(Class))~., data=TRAIN, ntree=ntrees, nodesize=ndsize[n], mtry=mtry[m])  #RFrPrb
      Prb=RFr$predicted
      Prb[abs(Prb)<10^-5]=min(Prb[abs(Prb)>10^-5])/2   #Deal with 0 probabilities
      Prb[abs(Prb-1)<10^-5]=(max(Prb[abs(Prb-1)>10^-5])+1)/2  #Deal with 1 probabilities
      OOBPreds[3,,m,n,1:length(mb)] <- Prb  #Note all entries for minbucket same since this isn't par. in RF
    #Predict test cases
      RFrPrb <- predict(RFr, newdata=TEST)
      RFrPrb[abs(RFrPrb)<10^-5]=min(RFrPrb[abs(RFrPrb)>10^-5])/2   #Deal with 0 probabilities
      RFrPrb[abs(RFrPrb-1)<10^-5]=(max(RFrPrb[abs(RFrPrb-1)>10^-5])+1)/2  #Deal with 1 probabilities
      TestPreds[3,,m,n,1:length(mb)] <-RFrPrb
      for(b in 1:length(mb)){
        CF=cforest(Class~., data=TRAIN, controls=cforest_unbiased(ntree=ntrees, mtry=mtry[m], minsplit=ndsize[n], minbucket=mb[b]*n))
        #note that increasing mincriterion moves predictions away from 0 but leads to huge nodes that aren't split-high bias
        CFPred=unlist(predict(CF, type="prob", OOB=T)) 
        Prb=CFPred[seq(2,2*nrow(TRAIN),2)]
        Prb[abs(Prb)<10^-5]=min(Prb[abs(Prb)>10^-5])/2   #Deal with 0 probabilities
        Prb[abs(Prb-1)<10^-5]=(max(Prb[abs(Prb-1)>10^-5])+1)/2  #Deal with 1 probabilities
        OOBPreds[4,,m,n,b]=Prb  #CFPrb
      #Predict Test cases
        CFPred=unlist(predict(CF, newdata=TEST, type="prob")) 
        CFPrb=CFPred[seq(2,2*nrow(TEST),2)]
        CFPrb[abs(CFPrb)<10^-5]=min(CFPrb[abs(CFPrb)>10^-5])/2   #Deal with 0 probabilities
        CFPrb[abs(CFPrb-1)<10^-5]=(max(CFPrb[abs(CFPrb-1)>10^-5])+1)/2  #Deal with 1 probabilities
        TestPreds[4,,m,n,b]<-CFPrb
      #CFaOOB Probabilities  
        Prb=UnweightCFOOB(CF,TRAIN,ntrees)[,2]  #CFaPrb
        Prb[abs(Prb)<10^-5]=min(Prb[abs(Prb)>10^-5])/2   #Deal with 0 probabilities
        Prb[abs(Prb-1)<10^-5]=(max(Prb[abs(Prb-1)>10^-5])+1)/2  #Deal with 1 probabilities
        OOBPreds[5,,m,n,b]=Prb
      #Test cases for CFA
        CFaPrb=UnweightCF(CF,TRAIN,TEST,ntrees)[,2]
        CFaPrb[abs(CFaPrb)<10^-5]=min(CFaPrb[abs(CFaPrb)>10^-5])/2   #Deal with 0 probabilities
        CFaPrb[abs(CFaPrb-1)<10^-5]=(max(CFaPrb[abs(CFaPrb-1)>10^-5])+1)/2  #Deal with 1 probabilities
        TestPreds[5,,m,n,b]<-CFaPrb
        OOBPreds[1,,m,n,b]=as.numeric(as.character(TRAIN$Class))  #Put true values in 1st spot
        TestPreds[1,,m,n,b]=as.numeric(as.character(TEST$Class))
        print(c(paste0("OOBm=: ", m),paste0("n=: ", n),paste0("b=: ", b)))   
      }
    }
  }
  return(list(OOBPreds, TestPreds))
}

