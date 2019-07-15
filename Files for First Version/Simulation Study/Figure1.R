#Code to run simulation and create graph for figure 1, showing calibration of equally and proportionally weighted 
#CF estimates.

library(plyr)
library(party)

#set model parameters
b0=-2.564
b1=1

#simulate 10,000 training cases and 1,000 test cases
set.seed(05210217)
size=11000 #Number of test and training cases

#simulate predictors
x1=rnorm(size,0,1)
x2=rnorm(size,0,1)
x3=rnorm(size,0,1)
x4=rnorm(size,0,1)
X=c(x1,x2,x3,x4)
dim(X) <- c(size, 4)
Class=c(rep(NA, size)) #vector for response
DATA=data.frame(X,Class)
trainprop=10/11
ntrees=500
eta=b0+b1*x1 #linear predictor
prb=exp(eta)/(1+exp(eta)) #true success probabilities
DATA$Class=rbinom(length(prb),1,prb) #simulate responses
DATA$Class=as.factor(DATA$Class) #set as 0-1 response

#split into test, training data
TRAIN=DATA[1:(size*trainprop), ] 
TEST=DATA[(size*trainprop+1):size, ]

#grow RF using default settings
CF=cforest(Class~., data=TRAIN, controls=cforest_unbiased(ntree=ntrees, mtry=2, minsplit=20, minbucket=7))
CFPred=unlist(predict(CF, newdata=TEST, type="prob")) 
CFPrb=CFPred[seq(2,2*nrow(TEST),2)] #proportionally weighted probabilities
#Grow RF using large terminal nodes (not used in paper)
CF2=cforest(Class~., data=TRAIN, controls=cforest_unbiased(ntree=ntrees, mtry=2, minsplit=100))
CFPred2=unlist(predict(CF2, newdata=TEST, type="prob")) 
CFPrb2=CFPred2[seq(2,2*nrow(TEST),2)]   #proportionally weighted probabilities
source("~/Core Aggregation Functions/CF_Weighting_Func.R")  
CFPrb_uw=UnweightCF(CF,TRAIN,TEST,ntrees)[,2]   #equally weighted probabilities
CFPrb_uw2=UnweightCF(CF2,TRAIN,TEST,ntrees)[,2]  #equally weighted probabilities
#save.image(file="CalSimFull.Rdata")

############################################################################################################
#Code to create graphic used in paper

load("CalSimFull.Rdata")
truth=prb[10001:11000] #vector with just true test probabilities

df=data.frame(truth, CFPrb, CFPrb_uw, CFPrb2, CFPrb_uw2)
library(ggplot2)
#default settings, prop. weighted
p1=ggplot(df, aes(truth, CFPrb))+ geom_point(alpha = 1/5)+xlim(0, 0.65)+ylim(0,0.65)+geom_abline(colour="red")+xlab("True Probability")+ylab("Estimated Probability")+ ggtitle("Proportional Weighting")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(size=14))
#default settings, equally weighted
p2=ggplot(df, aes(truth, CFPrb_uw))+ geom_point(alpha = 1/5)+xlim(0, 0.65)+ylim(0,0.65)+geom_abline(colour="red")+xlab("True Probability")+ylab("Estimated Probability")+ ggtitle("Equal Weighting")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(size=14))
#Not used in paper
#nodesize 100, prop. weighted
p3=ggplot(df, aes(truth, CFPrb2))+ geom_point(alpha = 1/5)+xlim(0, 0.65)+ylim(0,0.65)+geom_abline(colour="red")+xlab("True Probability")+ylab("Estimated Probability")+ ggtitle("Proportional Weighting")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(size=20))
#nodesize 100, equally weighted
p4=ggplot(df, aes(truth, CFPrb_uw2))+ geom_point(alpha = 1/5)+xlim(0, 0.65)+ylim(0,0.65)+geom_abline(colour="red")+xlab("True Probability")+ylab("Estimated Probability")+ ggtitle("Equal Weighting")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(size=20))

tiff('PW_SP.tiff', units="in", width=3, height=3, res=1200, compression = 'lzw')
p1
dev.off()


tiff('EW_SP.tiff', units="in", width=3, height=3, res=1200, compression = 'lzw')
p2
dev.off()

ggsave(filename = "PW_SP.eps", plot = p1,device = cairo_ps,width=3, height=3, units="in", dpi=1200)
ggsave(filename = "EW_SP.eps", plot = p2,device = cairo_ps, width=3, height=3, units="in", dpi=1200)

library(gridExtra)
ggsave(filename = "WeightingSP.eps", plot = grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                                                               p2 + theme(legend.position="none")+theme(axis.text=element_text(size=12)),
                                                               nrow=1))
       ,device = cairo_ps,width=7, height=4, units="in", dpi=1200)
