setwd("~/OneDrive - Lawrence University/Research/Aggregation Paper")
library(tidyr)
library(ggplot2)
library(dplyr)
library(GGally)
library(gridExtra)

################################################################################
#scatterplot matrix for one run on STEM data
load("STEM_m20.Rdata")
Probs <- t(as.matrix(Res[1,,]))  
#col 1 is true outcome
#col 2 is scaled prob
#col 3 is unscaled prob
#col 4 is tree vote prob
#col 5 is variance weighted prob
#col 6 is prob weighted by OOB accuracy


Probs <- t(as.matrix(Res[1,,]))  
Probsdf <- data.frame(Probs)
names(Probsdf) <- c("Truth", "Scaled", "Unscaled", "Tree voting", "Variance wt.", "OOB acc. wt.")
Probsdf$Truth <- factor(Probsdf$Truth)
df <- gather(Probsdf, Type, Estimate, "Scaled":"OOB acc. wt.")
df$minsplit <- "minsplit=20"

load("STEM_m1.Rdata")
Probs <- t(as.matrix(Res[1,,]))  
Probsdf <- data.frame(Probs)
names(Probsdf) <- c("Truth", "Scaled", "Unscaled", "Tree voting", "Variance wt.", "OOB acc. wt.")
Probsdf$Truth <- factor(Probsdf$Truth)
df1 <- gather(Probsdf, Type, Estimate, "Scaled":"OOB acc. wt.")
df1$minsplit <- "minsplit=1"

df <- rbind(df, df1)

#for some reason ordering gets messed up here but not below. This is a fix.
df$Type <- factor(df$Type, levels = c("Truth", "Scaled", "Unscaled", "Tree voting", "Variance wt.", "OOB acc. wt."),ordered = TRUE)

#boxplot of prob. estimates
p1 <- ggplot(data=df %>% filter(Type %in% c("Scaled", "Unscaled", "Tree voting","Variance wt.", "OOB acc. wt.")), aes(Type, Estimate)) + 
  geom_boxplot() +  facet_grid(cols=vars(minsplit)) + ylab("Estimated Probability") + xlab("Aggregation Method") 
p1 <- ggplot(data=df %>% filter(Type %in% c("Scaled", "Unscaled", "Tree voting")), aes(Type, Estimate)) + 
  geom_boxplot() +  facet_grid(cols=vars(minsplit)) + ylab("Estimated Probability") + xlab("Aggregation Method") 
p1 
ggsave(filename="STEM_BP.eps", plot = last_plot(), device = "eps", path = NULL,
       scale = 1, width = 6, height = 3, units = "in",
       dpi = 300, limitsize = TRUE)


#################Scatterplot Matrices
AddLine <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_abline(slope=1, color="red", ...) + theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  p
}

#scatterplot matrix of probability estimates
dev.off()
library(GGally)
load("STEM_m20.Rdata")
Probs <- t(as.matrix(Res[1,,]))  
load("STEM_m1.Rdata")
Probs2 <- t(as.matrix(Res[1,,]))
Probs[,4] <- Probs2[,4]  #use minsplit=1 predictions for tree-voting
Probsdf <- data.frame(Probs)
names(Probsdf) <- c("Truth", "Scaled", "Unscaled", "Tree voting", "Variance wt.", "OOB acc. wt.")
#ggpairs(Probsdf[,2:6]) + theme_bw() 
ggpairs(Probsdf, columns = 2:6, lower = list(continuous = AddLine)) + theme_bw()
ggsave(filename="STEM_SM.eps", plot = last_plot(), device = "eps", path = NULL,
       scale = 1, width = 6, height = 6, units = "in",
       dpi = 300, limitsize = TRUE)
dev.off()


##For supplementary Materials
load("STEMProbs.Rdata")
Probsdf$LR <- Probs[,7]
Probsdf$BST <- Probs[,8]
Probsdf$MARS <- Probs[,9]
names(Probsdf) <- c("Truth", "Sc", "Un", "TV", "Var.Wt.", "OOB wt.", "LR", "BST", "MARS")
ggpairs(Probsdf, columns = 2:8, lower = list(continuous = AddLine)) + theme_bw() + 
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) 
ggsave(filename="STEM_SM_Sup.eps", plot = last_plot(), device = "eps", path = NULL,
       scale = 1, width = 6, height = 6, units = "in",
       dpi = 300, limitsize = TRUE)


#boxplots for supplement
Probsdf_long <- gather(Probsdf, Type, Estimate, "Scaled":"MARS")
p <- ggplot(data=Probsdf_long %>% filter(Type %in% c("Sc", "Un", "TV", "Var.Wt.", "OOB wt.", "LR", "BST", "MARS")), 
            aes(Type, Estimate)) +geom_boxplot() + ylab("Estimated Probability") + xlab("Aggregation Method") +ylim(c(0,1))
p
ggsave(filename="STEM_BP_sup.eps", plot = last_plot(), device = "eps", path = NULL,
       scale = 1, width = 6, height = 3, units = "in",
       dpi = 300, limitsize = TRUE)


names(Probsdf) <- c("Truth", "Scaled", "Unscaled", "Tree voting", "Variance wt.", "OOB acc. wt.", "LR", "BST", "MARS")

#calibration plots
CalibrationPlot <- function(df, max, min, nbins, Type){
df <- df %>% select(c("Truth", Type))
df$Truth <- as.numeric(as.character(df$Truth))
binwidth <- (max - min) / nbins
names(df)[2] <- "Type"
df$bin <- floor((df[,2]-min)/binwidth) +1  
df1 <- df %>% group_by(bin) %>% summarize(MeanEst=mean(Type), PropLeft=mean(Truth), n=n())
p <- ggplot(data=df1 %>% filter(n>50), aes(MeanEst, PropLeft))+geom_point() + 
  geom_abline(slope=1, intercept=0)
return(p)
}

p1 <- CalibrationPlot(df=Probsdf, max=0.48, min=0, nbins=16, Type="Scaled") + ggtitle("Scaled") + theme(plot.title = element_text(hjust = 0.5))+ theme_bw() +xlab("Estimated Probability") + ylab("Proportion Leaving") +theme(plot.title = element_text(hjust = 0.5)) + xlim(c(0,.35))
p2 <- CalibrationPlot(df=Probsdf, max=0.48, min=0, nbins=16, Type="Unscaled") + ggtitle("Unscaled") + theme(plot.title = element_text(hjust = 0.5))+ theme_bw() +xlab("Estimated Probability") + ylab("Proportion Leaving")+theme(plot.title = element_text(hjust = 0.5)) + xlim(c(0,.35))
p3 <- CalibrationPlot(df=Probsdf, max=0.48, min=0, nbins=16, Type="Tree voting") + ggtitle("Tree voting") + theme(plot.title = element_text(hjust = 0.5))+ theme_bw() +xlab("Estimated Probability") + ylab("Proportion Leaving")+theme(plot.title = element_text(hjust = 0.5)) + xlim(c(0,.35))
p4 <- CalibrationPlot(df=Probsdf, max=0.48, min=0, nbins=16, Type="Variance wt.") + ggtitle("Variance wt.") + theme(plot.title = element_text(hjust = 0.5))+ theme_bw() +xlab("Estimated Probability") + ylab("Proportion Leaving")+theme(plot.title = element_text(hjust = 0.5)) + xlim(c(0,.35))
p5 <- CalibrationPlot(df=Probsdf, max=0.48, min=0, nbins=16, Type="OOB acc. wt.")+ ggtitle("OOB acc. wt.") + theme(plot.title = element_text(hjust = 0.5))+ theme_bw() +xlab("Estimated Probability") + ylab("Proportion Leaving")+theme(plot.title = element_text(hjust = 0.5)) + xlim(c(0,.35))
p6 <- CalibrationPlot(df=Probsdf, max=0.48, min=0, nbins=16, Type="LR")+ ggtitle("Logistic Regression") + theme(plot.title = element_text(hjust = 0.5))+ theme_bw() +xlab("Estimated Probability") + ylab("Proportion Leaving")+theme(plot.title = element_text(hjust = 0.5)) + xlim(c(0,.35))
p7 <- CalibrationPlot(df=Probsdf, max=0.48, min=0, nbins=16, Type="BST")+ ggtitle("Boosting") + theme(plot.title = element_text(hjust = 0.5))+ theme_bw() +xlab("Estimated Probability") + ylab("Proportion Leaving")+theme(plot.title = element_text(hjust = 0.5)) + xlim(c(0,.35))
p8 <- CalibrationPlot(df=Probsdf, max=0.48, min=0, nbins=16, Type="MARS")+ ggtitle("MARS") + theme(plot.title = element_text(hjust = 0.5))+ theme_bw() +xlab("Estimated Probability") + ylab("Proportion Leaving")+theme(plot.title = element_text(hjust = 0.5)) + xlim(c(0,.35))

p <- grid.arrange(p1, p2, p3, ncol=3) 
ggsave(filename="STEM_cal.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 9, height = 3, units = "in",
       dpi = 300, limitsize = TRUE)

p <- grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol=4) 
ggsave(filename="STEM_cal_sup.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 9, height = 6, units = "in",
       dpi = 300, limitsize = TRUE)


#################################################################################
#split using minsplit=20
load("Sim1_m20.Rdata")
Probs <- t(as.matrix(Res[1,,]))  
Probsdf <- data.frame(Probs)
names(Probsdf) <- c("Truth", "Scaled", "Unscaled", "Tree voting", "Variance wt.", "OOB acc. wt.")
Probsdf$Truth <- factor(Probsdf$Truth)
Probsdf$TrueProb <- prb[1001:2000]
df <- gather(Probsdf, Type, Estimate, "Scaled":"OOB acc. wt.", factor_key=TRUE)
df$diff <- df$Estimate - df$TrueProb
df$minsplit <- "minsplit=20"

#split using minsplit=1
load("Sim1_m1.Rdata")
Probs <- t(as.matrix(Res[1,,]))  
Probsdf <- data.frame(Probs)
names(Probsdf) <- c("Truth", "Scaled", "Unscaled", "Tree voting", "Variance wt.", "OOB acc. wt.")
Probsdf$Truth <- factor(Probsdf$Truth)
Probsdf$TrueProb <- prb[1001:2000]
df1 <- gather(Probsdf, Type, Estimate, "Scaled":"OOB acc. wt.", factor_key=TRUE)
df1$diff <- df1$Estimate - df1$TrueProb
df1$minsplit <- "minsplit=1"

df <- rbind(df, df1)

p <- ggplot(data=df %>% filter(Type %in% c("Scaled", "Unscaled", "Tree voting" )), aes(x=Estimate , y=TrueProb)) + facet_grid(cols = vars(Type))  + 
  geom_point(alpha = 0.3) +ylab("True Probability") + 
  geom_abline(slope=1, intercept=0, color="red") +facet_grid(cols=vars(Type), rows=vars(minsplit)) +theme_bw()
p
ggsave(filename="Sim1SP3_Faceted.pdf", plot = p, path = NULL,
       scale = 1, width = 9, height = 6, units = "in",
       dpi = 300, limitsize = TRUE)

## For supplement
p <- ggplot(data=df %>% filter(Type %in% c("Scaled", "Unscaled", "Tree voting", "Variance wt.", "OOB acc. wt."  )), aes(x=Estimate , y=TrueProb)) + facet_grid(cols = vars(Type))  + 
  geom_point(alpha = 0.3) +ylab("True Probability") + 
  geom_abline(slope=1, intercept=0, color="red") +facet_grid(cols=vars(Type), rows=vars(minsplit)) +theme_bw()
p
ggsave(filename="Sim1SP5_Faceted_sup.pdf", plot = p, path = NULL,
       scale = 1, width = 9, height = 6, units = "in",
       dpi = 300, limitsize = TRUE)



###################################################################################################
#Boxplots
#split using minsplit=20
load("Sim1_m20.Rdata")
Probs <- t(as.matrix(Res[1,,]))  
Probsdf <- data.frame(Probs)
names(Probsdf) <- c("Truth", "Scaled", "Unscaled", "Tree voting", "Variance wt.", "OOB acc. wt.")
Probsdf$Truth <- factor(Probsdf$Truth)
Probsdf$TrueProb <- prb[1001:2000]
df <- gather(Probsdf, Type, Estimate, "Scaled":"OOB acc. wt.", factor_key=TRUE)
df$diff <- df$Estimate - df$TrueProb
df$minsplit <- "minsplit=20"

#split using minsplit=1
load("Sim1_m1.Rdata")  #change to 1 
Probs <- t(as.matrix(Res[1,,]))  
Probsdf <- data.frame(Probs)
names(Probsdf) <- c("Truth", "Scaled", "Unscaled", "Tree voting", "Variance wt.", "OOB acc. wt.")
Probsdf$Truth <- factor(Probsdf$Truth)
Probsdf$TrueProb <- prb[1001:2000]
df2 <- gather(Probsdf, Type, Estimate, "Scaled":"OOB acc. wt.", factor_key=TRUE)
df2$diff <- df2$Estimate - df2$TrueProb
df2$minsplit <- "minsplit=1"

df <- rbind(df, df2)

p <- ggplot(data=df%>%filter(Type %in% c("Scaled", "Unscaled", "Tree voting")), aes(x=Type, y=diff)) +geom_boxplot() +
  ylab("Estimate-True Probability") +xlab("Response Class") + facet_grid(cols=vars(minsplit))
p
 ggsave(filename="Sim1BP_Faceted.eps", plot = p, device = "eps", path = NULL,
 scale = 1, width = 6, height = 3, units = "in", dpi = 300, limitsize = TRUE)

 #supplement
 p <- ggplot(data=df%>%filter(Type %in% c("Scaled", "Unscaled", "Tree voting", "Variance wt.", "OOB acc. wt.")), aes(x=Type, y=diff)) +geom_boxplot() +
   ylab("Estimate-True Probability") +xlab("Response Class") + facet_grid(cols=vars(minsplit))
 p
 ggsave(filename="Sim1BP_Faceted_sup.eps", plot = p, device = "eps", path = NULL,
        scale = 1, width = 9, height = 3, units = "in", dpi = 300, limitsize = TRUE)
 
 
             
####################################################################################################
load("Sim2_m20.Rdata")
class <- 3
Probs1 <- t(as.matrix(Res[1,,,class]))  
#col 1 is true outcome
#col 2 is scaled prob
#col 3 is unscaled prob
#col 4 is tree vote prob
#col 5 is variance weighted prob
#col 6 is prob weighted by OOB accuracy
#Last index represents probabilities for classes 1-3

#Scale true probs

Probs1df <- data.frame(Probs1)
names(Probs1df) <- c("Truth", "Scaled", "Unscaled", "Tree voting", "Variance wt.", "OOB acc. wt.")
Probs1df$Truth <- factor(Probs1df$Truth)
Probs1df$TrueProb <- Probs[1001:2000,class]
df <- gather(Probs1df, Type, Estimate, "Scaled":"OOB acc. wt.", factor_key=TRUE)


###Dataframe with predictions for all 3 classes, faceted by method and minsplit
load("Sim2_m1.Rdata")
df <- NA
for(class in 1:3){
Probs1 <- t(as.matrix(Res[1,,,class]))  
Probs1df <- data.frame(Probs1)
names(Probs1df) <- c("Truth", "Scaled", "Unscaled", "Tree voting", "Variance wt.", "OOB acc. wt.")
Probs1df$Truth <- factor(Probs1df$Truth)
Probs1df$TrueProb <- Probs[1001:2000,class]
Probs1df$Class <- class
df <- rbind(df, gather(Probs1df, Type, Estimate, "Scaled":"OOB acc. wt.", factor_key=TRUE))
}
df1 <- df[-1, ]
df1$diff <- df1$Estimate-df1$TrueProb
df1$minsplit <- "minsplit=1"
  
load("Sim2_m20.Rdata")
df <- NA
for(class in 1:3){
  Probs1 <- t(as.matrix(Res[1,,,class]))  
  Probs1df <- data.frame(Probs1)
  names(Probs1df) <- c("Truth", "Scaled", "Unscaled", "Tree voting", "Variance wt.", "OOB acc. wt.")
  Probs1df$Truth <- factor(Probs1df$Truth)
  Probs1df$TrueProb <- Probs[1001:2000,class]
  Probs1df$Class <- class
  df <- rbind(df, gather(Probs1df, Type, Estimate, "Scaled":"OOB acc. wt.", factor_key=TRUE))
}
df2 <- df[-1, ]
df2$diff <- df2$Estimate-df2$TrueProb
df2$minsplit <- "minsplit=20"

df <- rbind(df1, df2)
df$Class[df$Class==1] <- "Class 1"
df$Class[df$Class==2] <- "Class 2"
df$Class[df$Class==3] <- "Class 3"

p <- ggplot(data=df%>%filter(Type %in% c("Scaled", "Unscaled", "Tree voting")), aes(x=Class, y=diff)) + 
  geom_boxplot() + facet_grid(rows = vars(factor(Type)), cols = vars(minsplit)) + ylab("Estimate-True Probability") +xlab("Response Class")
ggsave(filename="Sim2BP_Faceted.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 6, height = 6, units = "in",
       dpi = 300, limitsize = TRUE)


p <- ggplot(data=df%>%filter(Type %in% c("Scaled", "Unscaled", "Tree voting", "Variance wt.", "OOB acc. wt.")), aes(x=Class, y=diff)) + 
  geom_boxplot() + facet_grid(rows = vars(factor(Type)), cols = vars(minsplit)) + ylab("Estimate-True Probability") +xlab("Response Class")
ggsave(filename="Sim2BP_Faceted_sup.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 6, height = 9, units = "in",
       dpi = 300, limitsize = TRUE)



#####################################################################################
#Real Data Results

load("Res_STEM.Rdata")
dim(Res_STEM[1,1][[1]])
Res <- t(as.matrix(Res_STEM[1,1][[1]][1,,]))

MeanPrbs <- array(dim=c(9,6,6))


for (fold in 1:9){
for (ndsz in 1:6){
 MeanPrbs[fold, ndsz, ] <- apply(t(as.matrix(Res_STEM[2,fold][[1]][ndsz,,])), 2 , mean)
}}

MeanPrbs <- apply(MeanPrbs, c(2,3), mean)


MeanPrbsdf <- data.frame(MeanPrbs[,2:6])
names(MeanPrbsdf) <- c("Scaled", "Unscaled", "Tree voting")
MeanPrbsdf$Nodesize <- c(1,5,20,100, 300, 500)
MeanPrbs_Long <- gather(MeanPrbsdf, Method, MeanProb, "Scaled":"Tree voting")
p1 <- ggplot(data=MeanPrbs_Long, aes(y=MeanProb, x=Nodesize, color= Method)) + geom_abline (intercept=0.0997, slope=0) + geom_point()+
  geom_line() + ylab("Mean Probability Estimate") + theme_bw() 
#ggsave(filename="STEM_CV_Cal.eps", plot = p, device = "eps", path = NULL,
#       scale = 1, width = 4, height = 3, units = "in",
#       dpi = 300, limitsize = TRUE)

##For supplement
names(MeanPrbsdf) <- c("Scaled", "Unscaled", "Tree voting", "Variance wt.", "OOB acc. wt.")
MeanPrbsdf$Nodesize <- c(1,5,20,100, 300, 500)
MeanPrbs_Long <- gather(MeanPrbsdf, Method, MeanProb, "Scaled":"OOB acc. wt.")
p1 <- ggplot(data=MeanPrbs_Long, aes(y=MeanProb, x=Nodesize, color= Method)) + geom_abline (intercept=0.0997, slope=0) + geom_point()+
  geom_line() + ylab("Mean Probability Estimate") + theme_bw() 
ggsave(filename="STEM_CV_Cal_sup.eps", plot = p, device = "eps", path = NULL,
      scale = 1, width = 6, height = 3, units = "in",
       dpi = 300, limitsize = TRUE)


logloss <- function(prob, truth){
prob[prob>0.999] <- 0.999
prob[prob<0.001] <- 0.001
    return(sum(-1*(log(prob)*(truth==1) + log(1-prob)*(truth==0))))
}


weightedlogloss <- function(prob, truth, alpha){
  prob[prob>0.999] <- 0.999
  prob[prob<0.001] <- 0.001
  return(sum(-1*(log(prob)*(truth==1)*alpha + log(1-prob)*(truth==0))))
}


LogLoss <- array(dim=c(9,6,6))

for (fold in 1:9){
  for (ndsz in 1:6){
      truth <- t(as.matrix(Res_STEM[2,fold][[1]][ndsz,1,]))
    LogLoss[fold, ndsz, ] <- apply(t(as.matrix(Res_STEM[2,fold][[1]][ndsz,,])), 2 , logloss, truth=truth)
  }}

LogLossMean <- apply(LogLoss, c(2,3), mean)

LogLossdf <- data.frame(LogLossMean[,2:6])
names(LogLossdf) <- c("Scaled", "Unscaled", "Tree voting")
LogLossdf$Nodesize <- c(1,5,20,100, 300, 500)
LogLoss_Long <- gather(LogLossdf, Method, LogLoss, "Scaled":"Tree voting")
p2 <- ggplot(data=LogLoss_Long, aes(y=LogLoss, x=Nodesize, color=Method))+geom_point() + 
  geom_line() + ylab("Log Loss") + ylim(c(290, 307)) + theme_bw() 
#ggsave(filename="LogLoss_STEM.eps", plot = p, device = "eps", path = NULL,
#       scale = 1, width = 4, height = 3, units = "in",
#       dpi = 300, limitsize = TRUE)

##Supplementary Info
names(LogLossdf)  <- c("Scaled", "Unscaled", "Tree voting", "Variance wt.", "OOB acc. wt.")
LogLossdf$Nodesize <- c(1,5,20,100, 300, 500)
LogLoss_Long <- gather(LogLossdf, Method, LogLoss, "Scaled":"OOB acc. wt.")
p2 <- ggplot(data=LogLoss_Long, aes(y=LogLoss, x=Nodesize, color=Method))+geom_point() + 
  geom_line() + ylab("Log Loss") + ylim(c(290, 307)) + theme_bw() 
ggsave(filename="LogLoss_STEM_sup.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 6, height = 3, units = "in",
       dpi = 300, limitsize = TRUE)



#################################################################################################
##GetResults that would have been chosen using CV

load("Res_STEM.Rdata")
dim(Res_STEM[1,1][[1]])
Res <- t(as.matrix(Res_STEM[1,1][[1]][1,,]))

logloss <- function(prob, truth){
  prob[prob>0.999] <- 0.999
  prob[prob<0.001] <- 0.001
  return(sum(-1*(log(prob)*(truth==1) + log(1-prob)*(truth==0))))
}

LogLossOOB <- array(dim=c(9,6,6))

for (fold in 1:9){
  for (ndsz in 1:6){
    truth <- t(as.matrix(Res_STEM[1,fold][[1]][ndsz,1,]))
    LogLossOOB[fold, ndsz, ] <- apply(t(as.matrix(Res_STEM[1,fold][[1]][ndsz,,])), 2 , logloss, truth=truth)
  }}

ChosenNodesize <- array(dim=c(9,6))

for(fold in 1:9){
ChosenNodesize[fold,] <- apply(LogLossOOB[fold, , ], 2, which.min)  
}


#Get results from parameter chosen using OOB CV

SelectedLL <- array(dim=c(9,6))

for(fold in 1:9){
for(tech in 1:6){
    SelectedLL[fold,tech] <- LogLoss[fold, ChosenNodesize[fold, tech], tech]
}
}

apply(SelectedLL, 2, mean)

############################################################################################

weightedlogloss <- function(prob, truth, alpha){
  prob[prob>0.999] <- 0.999
  prob[prob<0.001] <- 0.001
  return(sum(-1*(log(prob)*(truth==1)*alpha + log(1-prob)*(truth==0))))
}

avec <- c(1,2,3,4,5,6,7,8,9,10, 100, 1000)
WtLogLoss <- array(dim=c(length(avec),9,6,6))

for (a in 1:length(avec)){
alpha <-  avec[a]
for (fold in 1:9){
  for (ndsz in 1:6){
    truth <- t(as.matrix(Res_STEM[2,fold][[1]][ndsz,1,]))
    WtLogLoss[a, fold, ndsz, ] <- apply(t(as.matrix(Res_STEM[2,fold][[1]][ndsz,,])), 2 , weightedlogloss, truth=truth, alpha=alpha)
  }}}

LogLossOOB <- array(dim=c(length(avec), 9,6,6))

for (a in 1:length(avec)){
  alpha <-  avec[a]
  for (fold in 1:9){
  for (ndsz in 1:6){
    truth <- as.matrix(Res_STEM[1,fold][[1]][ndsz,1,])
    LogLossOOB[a, fold, ndsz, ] <- apply(t(as.matrix(Res_STEM[1,fold][[1]][ndsz,,])), 2 , weightedlogloss, truth=truth, alpha=alpha)
  }}}

ChosenNodesize <- array(dim=c(length(avec),9,6))

for (a in 1:length(avec)){
for(fold in 1:9){
  ChosenNodesize[a,fold,] <- apply(LogLossOOB[a,fold, , ], 2, which.min)  
}}

ndsizevec <- c(1,5,20,100,300,500)
NodesizeVal <- array(dim=c(length(avec),9,6))
for (tech in 1:6){
for (a in 1:length(avec)){
  for(fold in 1:9){
    NodesizeVal[a,fold,tech] <- ndsizevec[ChosenNodesize[a,fold,tech]]
  }}}
  
  
#Get results from parameter chosen using OOB CV
SelectedLL <- array(dim=c(length(avec), 9,6))

for (a in 1:length(avec)){
for(fold in 1:9){
  for(tech in 1:6){
    SelectedLL[a, fold,tech] <- WtLogLoss[a, fold, ChosenNodesize[a,fold, tech], tech]
  }}}

#Values for each of 9 folds using alpha=1
round(SelectedLL[1,,],1)

#Matrix with average log loss for each alpha and technique
WtLogLossSel <- apply(SelectedLL, c(1,3), mean) # alpha by technique

#Choice of nodesize value
NodesizeValMean <- apply(NodesizeVal, c(1,3), mean)
NodesizeValMean[c(1,2,3,4,5,10,11,12),]

WtLogLossdf <- data.frame(WtLogLossSel[,2:4])
names(WtLogLossdf) <- c("Scaled", "Unscaled", "Tree voting")
RatioMat <- apply(WtLogLossdf, 1, function(x)(x/min(x)))
Ratiodf <- data.frame(t(RatioMat))
Ratiodf <- gather(Ratiodf, Type, Ratio)
Ratiodf$Alpha <- rep(c(1, 2, 3, 4, 5, 6,7,8,9,10, 100, 1000), 3)
p3 <- ggplot(data=Ratiodf, aes(y=Ratio, x=Alpha, color=Type))+geom_point()+geom_line()+
  xlab(expression(alpha)) +ylab("Log Loss Ratio to Best Method") + theme_bw() +xlim(c(1,5)) 
#ggsave(filename="RatioSTEM.eps", plot = p, device = "eps", path = NULL,
#       scale = 1, width = 6, height = 3, units = "in",
#       dpi = 300, limitsize = TRUE)

###################################################################################
##CCD CV 
load("Res_CCD1.Rdata")

MeanPrbs <- array(dim=c(10,6,6))

for (fold in 1:10){
  ndsz <- 1
  load("Res_CCD1.Rdata")
    MeanPrbs[fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[2,fold][[1]][1,,])), 2 , mean)
    ndsz <- 2
    load("Res_CCD2.Rdata")
    MeanPrbs[fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[2,fold][[1]][1,,])), 2 , mean)
    ndsz <- 3
    load("Res_CCD3.Rdata")
    MeanPrbs[fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[2,fold][[1]][1,,])), 2 , mean)
    ndsz <- 4
    load("Res_CCD4.Rdata")
    MeanPrbs[fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[2,fold][[1]][1,,])), 2 , mean)
    ndsz <- 5
    load("Res_CCD5.Rdata")
    MeanPrbs[fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[2,fold][[1]][1,,])), 2 , mean)
    ndsz <- 6
    load("Res_CCD6.Rdata")
    MeanPrbs[fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[2,fold][[1]][1,,])), 2 , mean)
  }

MeanPrbs <- apply(MeanPrbs, c(2,3), mean)

MeanPrbsdf <- data.frame(MeanPrbs[,2:6])
names(MeanPrbsdf) <- c("Scaled", "Unscaled", "Tree voting")
MeanPrbsdf$Nodesize <- c(1,5,20,100, 300, 500)
MeanPrbs_Long <- gather(MeanPrbsdf, Method, MeanProb, "Scaled":"Tree voting")
p1b <- ggplot(data=MeanPrbs_Long, aes(y=MeanProb, x=Nodesize, color=Method)) + geom_abline (intercept=0.2212, slope=0) + geom_point()+
  geom_line() + ylab("Mean Probability Estimate") + theme_bw() 
#ggsave(filename="STEM_CV_Cal.eps", plot = p, device = "eps", path = NULL,
#       scale = 1, width = 4, height = 3, units = "in",
#       dpi = 300, limitsize = TRUE)


#Combine graphics from STEM and CCD datasets
library(gridExtra)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p1)
p <- grid.arrange(arrangeGrob(p1b + theme(legend.position="none")+ ggtitle("CCD") +theme(plot.title = element_text(hjust = 0.5)),
                              p1 + theme(legend.position="none") + ggtitle("STEM") +theme(plot.title = element_text(hjust = 0.5))
                              + theme(axis.text=element_text(size=12)),
                              nrow=1),
                  mylegend, nrow=2,heights=c(10,3)) 

ggsave(filename="Comb_CV_Cal.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 6, height = 4, units = "in",
       dpi = 300, limitsize = TRUE)




LogLoss <- array(dim=c(10,6,6))
LogLossOOB <- array(dim=c(10,6,6))

ndsz <- 1
load("Res_CCD1.Rdata")
for (fold in 1:10){
  truth <- t(as.matrix(Res_CCD[2,fold][[1]][1,1,]))
  LogLoss[fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[2,fold][[1]][1,,])), 2 , logloss, truth=truth)
  truth <- t(as.matrix(Res_CCD[1,fold][[1]][1,1,]))
  LogLossOOB[fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[1,fold][[1]][1,,])), 2 , logloss, truth=truth)
}
ndsz <- 2
load("Res_CCD2.Rdata")
for (fold in 1:10){
  truth <- t(as.matrix(Res_CCD[2,fold][[1]][1,1,]))
  LogLoss[fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[2,fold][[1]][1,,])), 2 , logloss, truth=truth)
  truth <- t(as.matrix(Res_CCD[1,fold][[1]][1,1,]))
  LogLossOOB[fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[1,fold][[1]][1,,])), 2 , logloss, truth=truth)
}
ndsz <- 3
load("Res_CCD3.Rdata")
for (fold in 1:10){
  truth <- t(as.matrix(Res_CCD[2,fold][[1]][1,1,]))
  LogLoss[fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[2,fold][[1]][1,,])), 2 , logloss, truth=truth)
  truth <- t(as.matrix(Res_CCD[1,fold][[1]][1,1,]))
  LogLossOOB[fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[1,fold][[1]][1,,])), 2 , logloss, truth=truth)
}
ndsz <- 4
load("Res_CCD4.Rdata")
for (fold in 1:10){
  truth <- t(as.matrix(Res_CCD[2,fold][[1]][1,1,]))
  LogLoss[fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[2,fold][[1]][1,,])), 2 , logloss, truth=truth)
  truth <- t(as.matrix(Res_CCD[1,fold][[1]][1,1,]))
  LogLossOOB[fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[1,fold][[1]][1,,])), 2 , logloss, truth=truth)
}
ndsz <- 5
load("Res_CCD5.Rdata")
for (fold in 1:10){
  truth <- t(as.matrix(Res_CCD[2,fold][[1]][1,1,]))
  LogLoss[fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[2,fold][[1]][1,,])), 2 , logloss, truth=truth)
  truth <- t(as.matrix(Res_CCD[1,fold][[1]][1,1,]))
  LogLossOOB[fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[1,fold][[1]][1,,])), 2 , logloss, truth=truth)
}
ndsz <- 6
load("Res_CCD6.Rdata")
for (fold in 1:10){
  truth <- t(as.matrix(Res_CCD[2,fold][[1]][1,1,]))
  LogLoss[fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[2,fold][[1]][1,,])), 2 , logloss, truth=truth)
  truth <- t(as.matrix(Res_CCD[1,fold][[1]][1,1,]))
  LogLossOOB[fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[1,fold][[1]][1,,])), 2 , logloss, truth=truth)
}
LogLossMean <- apply(LogLoss, c(2,3), mean)

LogLossdf <- data.frame(LogLossMean[,2:6])
names(LogLossdf) <- c("Scaled", "Unscaled", "Tree voting")
LogLossdf$Nodesize <- c(1,5,20,100, 300, 500)
LogLoss_Long <- gather(LogLossdf, Method, LogLoss, "Scaled":"Tree voting")
p2b <- ggplot(data=LogLoss_Long, aes(y=LogLoss, x=Nodesize, color=Method))+geom_point() + ylim(c(1275,1600)) +
  geom_line() + ylab("Log Loss") + theme_bw()


mylegend<-g_legend(p2)
p <- grid.arrange(arrangeGrob(p2b + theme(legend.position="none")+ ggtitle("CCD") +theme(plot.title = element_text(hjust = 0.5)),
                              p2 + theme(legend.position="none") + ggtitle("STEM") +theme(plot.title = element_text(hjust = 0.5))
                              + theme(axis.text=element_text(size=12)),
                              nrow=1),
                  mylegend, nrow=2,heights=c(10,3)) 

ggsave(filename="Comb_LL_Cal.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 6, height = 4, units = "in",
       dpi = 300, limitsize = TRUE)


ChosenNodesize <- array(dim=c(10,6))

for(fold in 1:10){
  ChosenNodesize[fold,] <- apply(LogLossOOB[fold, , ], 2, which.min)  
}


#Get results from parameter chosen using OOB CV
SelectedLL <- array(dim=c(10,6))

for(fold in 1:10){
  for(tech in 1:6){
    SelectedLL[fold,tech] <- LogLoss[fold, ChosenNodesize[fold, tech], tech]
  }
}

apply(SelectedLL, 2, mean)

############################################################################################

weightedlogloss <- function(prob, truth, alpha){
  prob[prob>0.999] <- 0.999
  prob[prob<0.001] <- 0.001
  return(sum(-1*(log(prob)*(truth==1)*alpha + log(1-prob)*(truth==0))))
}

avec <- c(1,2,3,4,5,6,7,8,9,10, 100, 1000)
WtLogLoss <- array(dim=c(length(avec),10,6,6))
LogLossOOB <- array(dim=c(length(avec), 10,6,6))

for (a in 1:length(avec)){
  alpha <-  avec[a]
  for (fold in 1:10){
    ndsz <- 1
    load("Res_CCD1.Rdata")
    truth <- t(as.matrix(Res_CCD[2,fold][[1]][1,1,]))
    WtLogLoss[a, fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[2,fold][[1]][1,,])), 2 , weightedlogloss, truth=truth, alpha=alpha)
    truth <- t(as.matrix(Res_CCD[1,fold][[1]][1,1,]))
    LogLossOOB[a, fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[1,fold][[1]][1,,])), 2 , weightedlogloss, truth=truth, alpha=alpha)
    ndsz <- 2
    load("Res_CCD2.Rdata")
    truth <- t(as.matrix(Res_CCD[2,fold][[1]][1,1,]))
    WtLogLoss[a, fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[2,fold][[1]][1,,])), 2 , weightedlogloss, truth=truth, alpha=alpha)
    truth <- t(as.matrix(Res_CCD[1,fold][[1]][1,1,]))
    LogLossOOB[a, fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[1,fold][[1]][1,,])), 2 , weightedlogloss, truth=truth, alpha=alpha)
    ndsz <- 3
    load("Res_CCD3.Rdata")
    truth <- t(as.matrix(Res_CCD[2,fold][[1]][1,1,]))
    WtLogLoss[a, fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[2,fold][[1]][1,,])), 2 , weightedlogloss, truth=truth, alpha=alpha)
    truth <- t(as.matrix(Res_CCD[1,fold][[1]][1,1,]))
    LogLossOOB[a, fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[1,fold][[1]][1,,])), 2 , weightedlogloss, truth=truth, alpha=alpha)
    ndsz <- 4
    load("Res_CCD4.Rdata")
    truth <- t(as.matrix(Res_CCD[2,fold][[1]][1,1,]))
    WtLogLoss[a, fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[2,fold][[1]][1,,])), 2 , weightedlogloss, truth=truth, alpha=alpha)
    truth <- t(as.matrix(Res_CCD[1,fold][[1]][1,1,]))
    LogLossOOB[a, fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[1,fold][[1]][1,,])), 2 , weightedlogloss, truth=truth, alpha=alpha)
    ndsz <- 5
    load("Res_CCD5.Rdata")
    truth <- t(as.matrix(Res_CCD[2,fold][[1]][1,1,]))
    WtLogLoss[a, fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[2,fold][[1]][1,,])), 2 , weightedlogloss, truth=truth, alpha=alpha)
    truth <- t(as.matrix(Res_CCD[1,fold][[1]][1,1,]))
    LogLossOOB[a, fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[1,fold][[1]][1,,])), 2 , weightedlogloss, truth=truth, alpha=alpha)
    ndsz <- 6
    load("Res_CCD6.Rdata")
    truth <- t(as.matrix(Res_CCD[2,fold][[1]][1,1,]))
    WtLogLoss[a, fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[2,fold][[1]][1,,])), 2 , weightedlogloss, truth=truth, alpha=alpha)
    truth <- t(as.matrix(Res_CCD[1,fold][[1]][1,1,]))
    LogLossOOB[a, fold, ndsz, ] <- apply(t(as.matrix(Res_CCD[1,fold][[1]][1,,])), 2 , weightedlogloss, truth=truth, alpha=alpha)
  }
}


ChosenNodesize <- array(dim=c(length(avec),10,6))

for (a in 1:length(avec)){
  for(fold in 1:10){
    ChosenNodesize[a,fold,] <- apply(LogLossOOB[a,fold, , ], 2, which.min)  
  }}

ndsizevec <- c(1,5,20,100,300,500)
NodesizeVal <- array(dim=c(length(avec),10,6))
for (tech in 1:6){
  for (a in 1:length(avec)){
    for(fold in 1:10){
      NodesizeVal[a,fold,tech] <- ndsizevec[ChosenNodesize[a,fold,tech]]
    }}}


#Get results from parameter chosen using OOB CV
SelectedLL <- array(dim=c(length(avec), 10,6))

for (a in 1:length(avec)){
  for(fold in 1:10){
    for(tech in 1:6){
      SelectedLL[a, fold,tech] <- WtLogLoss[a, fold, ChosenNodesize[a,fold, tech], tech]
    }}}

#Values for each of 10 folds using alpha=1
round(SelectedLL[1,,],1)

#Matrix with average log loss for each alpha and technique
WtLogLossSel <- apply(SelectedLL, c(1,3), mean) # alpha by technique

#Choice of nodesize value
NodesizeValMean <- apply(NodesizeVal, c(1,3), mean)
NodesizeValMean[c(1,2,3,4,5,10,11,12),]

WtLogLossdf <- data.frame(WtLogLossSel[,2:4])
names(WtLogLossdf) <- c("Scaled", "Unscaled", "Tree voting")
RatioMat <- apply(WtLogLossdf, 1, function(x)(x/min(x)))
Ratiodf <- data.frame(t(RatioMat))
Ratiodf <- gather(Ratiodf, Type, Ratio)
Ratiodf$Alpha <- rep(c(1, 2, 3, 4, 5, 6,7,8,9,10, 100, 1000), 3)
p3b <- ggplot(data=Ratiodf, aes(y=Ratio, x=Alpha, color=Type))+geom_point()+geom_line()+
  xlab(expression(alpha)) +ylab("Log Loss Ratio to Best Method") + theme_bw() +xlim(c(1,5))
#ggsave(filename="RatioCCD.eps", plot = p, device = "eps", path = NULL,
#       scale = 1, width = 6, height = 3, units = "in",
#       dpi = 300, limitsize = TRUE)


mylegend<-g_legend(p2)
p <- grid.arrange(arrangeGrob(p3b + theme(legend.position="none")+ ggtitle("CCD") +theme(plot.title = element_text(hjust = 0.5)),
                              p3 + theme(legend.position="none") + ggtitle("STEM") +theme(plot.title = element_text(hjust = 0.5))
                              + theme(axis.text=element_text(size=12)),
                              nrow=1),
                  mylegend, nrow=2,heights=c(10,3)) 

ggsave(filename="Comb_Ratio_Cal.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 6, height = 4, units = "in",
       dpi = 300, limitsize = TRUE)














###############################################################################################
#Simulation 1 results

load("Res_Sim1.Rdata")
dim(Res_Sim1[1,1][[1]])
Res <- t(as.matrix(Res_Sim1[1,1][[1]][1,,]))

MeanPrbs <- array(dim=c(100,6,6))


for (rep in 1:100){
  for (ndsz in 1:6){
    MeanPrbs[rep, ndsz, ] <- apply(t(as.matrix(Res_Sim1[2,rep][[1]][ndsz,,])), 2 , mean)
  }}

MeanPrbs <- apply(MeanPrbs, c(2,3), mean)


MeanPrbsdf <- data.frame(MeanPrbs[,2:6])
names(MeanPrbsdf) <- c("Scaled", "Unscaled", "TreeVoting", "VarianceWt", "OOBPredWt")
MeanPrbsdf$Nodesize <- c(1,5,20,100, 300, 500)
MeanPrbs_Long <- gather(MeanPrbsdf, Method, MeanProb, Scaled:OOBPredWt)
ggplot(data=MeanPrbs_Long, aes(y=MeanProb, x=Nodesize, color=Method))+geom_point()+geom_line() + ylab("Mean Probability Estimate")
ggplot(data=MeanPrbs_Long, aes(y=MeanProb, x=Nodesize, color=Method))+geom_point()+geom_line()+ylim(c(0.08, 0.11))+ ylab("Mean Probability Estimate")



logloss <- function(prob, truth){
  prob[prob>0.999] <- 0.999
  prob[prob<0.001] <- 0.001
  return(sum(-1*(log(prob)*(truth==1) + log(1-prob)*(truth==0))))
}


weightedlogloss <- function(prob, truth){
  prob[prob>0.999] <- 0.999
  prob[prob<0.001] <- 0.001
  return(sum(-1*(log(prob)*(truth==1)*10 + log(1-prob)*(truth==0))))
}


LogLoss <- array(dim=c(100,6,6))

for (rep in 1:100){
  for (ndsz in 1:6){
    truth <- t(as.matrix(Res_Sim1[2,rep][[1]][ndsz,1,]))
    LogLoss[rep, ndsz, ] <- apply(t(as.matrix(Res_Sim1[2,rep][[1]][ndsz,,])), 2 , logloss, truth=truth)
  }}

LogLoss <- apply(LogLoss, c(2,3), mean)

LogLossdf <- data.frame(LogLoss[,2:6])
names(LogLossdf) <- c("Scaled", "Unscaled", "TreeVoting", "VarianceWt", "OOBPredWt")
LogLossdf$Nodesize <- c(1,5,20,100, 300, 500)
LogLoss_Long <- gather(LogLossdf, Method, LogLoss, Scaled:OOBPredWt)
ggplot(data=LogLoss_Long, aes(y=LogLoss, x=Nodesize, color=Method))+geom_point()+geom_line() + ylab("Log Loss")
ggplot(data=LogLoss_Long, aes(y=LogLoss, x=Nodesize, color=Method))+geom_point()+geom_line()+ ylab("Log Loss") + ylim(c(280, 420))


WtLogLoss <- array(dim=c(100,6,6))

for (rep in 1:100){
  for (ndsz in 1:6){
    truth <- t(as.matrix(Res_Sim1[2,rep][[1]][ndsz,1,]))
    WtLogLoss[rep, ndsz, ] <- apply(t(as.matrix(Res_Sim1[2,rep][[1]][ndsz,,])), 2 , weightedlogloss, truth=truth)
  }}

WtLogLoss <- apply(WtLogLoss, c(2,3), mean)

WtLogLossdf <- data.frame(WtLogLoss[,2:6])
names(WtLogLossdf) <- c("Scaled", "Unscaled", "TreeVoting", "VarianceWt", "OOBPredWt")
WtLogLossdf$Nodesize <- c(1,5,20,100, 300, 500)
WtLogLoss_Long <- gather(WtLogLossdf, Method, WtLogLoss, Scaled:OOBPredWt)
ggplot(data=WtLogLoss_Long, aes(y=WtLogLoss, x=Nodesize, color=Method))+geom_point()+geom_line() + ylab("Log Loss")
ggplot(data=WtLogLoss_Long, aes(y=WtLogLoss, x=Nodesize, color=Method))+geom_point()+geom_line()+ ylab("Log Loss") + ylim(c(2000, 4000))

#####################################################################################################
################################################################################
#scatterplot matrix for one run on regression Ames Housing data
load("Ames_House_m20.Rdata")

Predsdf <- data.frame(PredSc, PredUnsc)
Predsdf$Truth <- TEST$Y

df <- gather(Predsdf, Type, Estimate, "PredSc":"PredUnsc", factor_key=TRUE)

#boxplot of prob. estimates
p1 <- ggplot(data=df[df$Type != "Truth", ], aes(Type, Estimate)) + geom_boxplot()
p1 + ylab("Estimated Price") + xlab("Aggregation Method")

# preds plotted against each other
p <- ggplot(data=Predsdf , aes(PredUnsc, PredSc)) + geom_point() +
  geom_abline(slope=1, intercept=0, colour="red") +xlab("Unscaled") +ylab("Scaled") + theme_bw()
p 
ggsave(filename="Ames_SP.eps", plot = p, device = "eps", path = NULL,
       scale = 1, width = 3, height = 3, units = "in",
       dpi = 300, limitsize = TRUE)

Predsdf_long <- gather(Predsdf, Type, Estimate, "PredSc":"PredUnsc")

Predsdf_long[Predsdf_long=="PredSc"] <- "Scaled Aggregation"
Predsdf_long[Predsdf_long=="PredUnsc"] <- "Unscaled Aggregation"


p1 <- ggplot(data=Predsdf_long , aes(x=Estimate, y=Truth)) + geom_point() +
  geom_abline(slope=1, intercept=0, colour="red") + 
  xlab("Predicted Price")+ ylab("True Price") + facet_grid(cols=vars(Type)) +
  theme(plot.title = element_text(hjust = 0.5)) + theme_bw()
ggsave(filename="Ames_cal.eps", plot = p1, device = "eps", path = NULL,
       scale = 1, width = 6, height = 3, units = "in",
       dpi = 300, limitsize = TRUE)

