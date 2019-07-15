##Functions for calculating predictions using different aggregation approaches.


logloss <- function(prob, truth){
  return(sum(-1*(log(prob)*(truth==1) + log(1-prob)*(truth==0))))
}

#functon to take in test case, make prediction where each tree casts a 0-1 vote
DefPred2Class <- function(Forest, case, TEST, TRAIN, nd, lw, np){
  ndc <- predict(Forest, newdata = TEST[case,,drop=FALSE], type = "node")
  m <- numeric(length(lw))
  for (tree in 1:length(lw)) {
    #compute probabilities in each tree
    #in tree i, divide number of 1's in each node by number of cases in each node
    #lw contains 0-1's for whether case is used to grow tree
    np[[tree]] <- tapply(lw[[tree]] * TRAIN$Y, nd[[tree]], sum) /  
      tapply(lw[[tree]], nd[[tree]], sum)
    #if prop. in node exceeds 0.5, count as 1, if =0.5, flip coin, otherwise count as 0.
    m[tree] <- np[[tree]][as.character(ndc[tree])] > 0.5 +
      (np[[tree]][as.character(ndc[tree])] == 0.5)*rbinom(1,1,.5)
  }
  return(mean(m))  
}

#functon to take in test case, make prediction where each tree casts a 0-1 vote
DefPredOOB2Class <- function(Forest, case, TEST, TRAIN, nd, lw, np, OOBcases){
  ndc <- predict(Forest, newdata = TRAIN[case,,drop=FALSE], type = "node")
  m <- numeric(length(lw))
  for (tree in which(OOBcases[,case]==1)) { #only look at trees where case is OOB
    #compute probabilities in each tree
    #in tree i, divide number of 1's in each node by number of cases in each node
    #lw contains 0-1's for whether case is used to grow tree
    np[[tree]] <- tapply(lw[[tree]] * TRAIN$Y, nd[[tree]], sum) /  
      tapply(lw[[tree]], nd[[tree]], sum)
    #if prop. in node exceeds 0.5, count as 1, if =0.5, flip coin, otherwise count as 0.
    m[tree] <- np[[tree]][as.character(ndc[tree])] > 0.5 +
      (np[[tree]][as.character(ndc[tree])] == 0.5)*rbinom(1,1,.5)
  }
  return(sum(m)/sum(OOBcases[,case]==1))  
}

#if length 1, just take value, otherwise sample
sampfunc <- function(x){if(length(x)==1) x else sample(x,size=1)}


#function to get mose frequent outcome (other than 0, which is OOB case)
#classes must be lableled 1,2,3,4...
#in event of a tie, will randomly pick a class from those tied
getmode <- function(v) {
  v <- v[v!=0]  #get rid of 0's as these represent OOB cases
  uniqv <- unique(v)
  counts <- tabulate(match(v, uniqv))
  class <- sampfunc(which(counts==max(counts)))
  return(class)
}

#return index of mode based on counts
#if more than 1, randomly select 1
getwhichmode <- function(counts) {
  class <- sampfunc(which(counts==max(counts)))
  return(class)
}



# For a binary classification problem, weight trees by variance in terminal node
WtbyVar2Class <- function(Forest, case, TEST, TRAIN, nd, lw, np){
  ndc <- predict(Forest, newdata = TEST[case,,drop=FALSE], type = "node")
  m <- numeric(length(lw))
  v <- numeric(length(lw))
  for (tree in 1:length(lw)) {
    np[[tree]] <- tapply(lw[[tree]] * TRAIN$Y, nd[[tree]], sum) /  #prediction associated with each node
      tapply(lw[[tree]], nd[[tree]], sum)
    ones <- tapply(lw[[tree]] * TRAIN$Y, nd[[tree]], sum)  #zeros in each node
    zeros <- tapply(lw[[tree]], nd[[tree]], sum)  - ones # 1's in each node
    nv <- (np[[tree]]^2*zeros+(1-np[[tree]])^2*ones) / (zeros + ones)  #variance in each node
    m[tree] <- np[[tree]][as.character(ndc[tree])]  #tree prediction for a specific case
    v[tree] <- nv[as.character(ndc[tree])]  #variance in node
  }
  Wtm <- sum(m*(1-v))/sum(1-v)  #weighted mean of tree predictions with weights determined by variance in terminal nodes
  return(mean(Wtm))  
}

# For a binary classification problem, weight trees by variance in terminal node
WtbyVarOOB2Class <- function(Forest, case, TEST, TRAIN, nd, lw, np, OOBcases){
  ndc <- predict(Forest, newdata = TRAIN[case,,drop=FALSE], type = "node")
  m <- numeric(length(lw))
  v <- numeric(length(lw))
  for (tree in which(OOBcases[,case]==1)) { #only look at trees where case is OOB
    np[[tree]] <- tapply(lw[[tree]] * TRAIN$Y, nd[[tree]], sum) /  #prediction associated with each node
      tapply(lw[[tree]], nd[[tree]], sum)
    ones <- tapply(lw[[tree]] * TRAIN$Y, nd[[tree]], sum)  #zeros in each node
    zeros <- tapply(lw[[tree]], nd[[tree]], sum)  - ones # 1's in each node
    nv <- (np[[tree]]^2*zeros+(1-np[[tree]])^2*ones) / (zeros + ones)  #variance in each node
    m[tree] <- np[[tree]][as.character(ndc[tree])]  #tree prediction for a specific case
    v[tree] <- nv[as.character(ndc[tree])]  #variance in node
  }
  wt <- (1-v)*OOBcases[,case]
  Wtm <- sum(m*wt)/sum(wt)  #weighted mean of tree predictions with weights determined by variance in terminal nodes
  return(Wtm)  
}


#run each OOB case through each tree to get prediction, return prediction of each tree
GetOOBPred <- function(Forest, traincase, TEST, TRAIN, nd, lw, np){
  ndc <- predict(Forest, newdata = TRAIN[traincase,,drop=FALSE], type = "node")
  m <- numeric(length(lw))
  v <- numeric(length(lw))
  for (tree in 1:length(lw)) {
    np[[tree]] <- tapply(lw[[tree]] * TRAIN$Y, nd[[tree]], sum) /  #prediction associated with each node
      tapply(lw[[tree]], nd[[tree]], sum)
    m[tree] <- np[[tree]][as.character(ndc[tree])] > 0.5 +
      (np[[tree]][as.character(ndc[tree])] == 0.5)*rbinom(1,1,.5)  
  }
  return(m)  
}

WtbyOOBPerf <- function(Forest, case, TEST, TRAIN, nd, lw, np, TreeMC){
  ndc <- predict(Forest, newdata = TEST[case,,drop=FALSE], type = "node")
  m <- numeric(length(lw))
  v <- numeric(length(lw))
  for (tree in 1:length(lw)) {
    np[[tree]] <- tapply(lw[[tree]] * TRAIN$Y, nd[[tree]], sum) /  #prediction associated with each node
      tapply(lw[[tree]], nd[[tree]], sum)
    m[tree] <- np[[tree]][as.character(ndc[tree])]  #tree prediction for a specific case
  }
  Wtm <- sum(m*(1-TreeMC))/sum(1-TreeMC)  #weighted mean of tree predictions with weights determined by variance in terminal nodes
  return(mean(Wtm))  
}


CalculatePredictions <- function(fold){
  TRAIN <- DATA[-Folds[,fold],]
  TEST <- DATA[Folds[,fold],]
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
    print(fold)
  }
  return(list(OOBRes, Res))
}  
###################################################################################################
#Code for simulated rather than real datasets and 3-class predictions

library(mvtnorm)

Generate3ClassData <- function(size){ #size in number of training and test cases combined
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
DATA <- data.frame(X, Y)
return(DATA)
}

#run each OOB case through each tree to get prediction, return prediction from each tree
GetOOBPred3Class <- function(Forest, traincase, TEST, TRAIN, nd, lw, np){
  ndc <- predict(Forest, newdata = TRAIN[traincase,,drop=FALSE], type = "node")
  m <- numeric(length(lw))
  v <- numeric(length(lw))
  for (tree in 1:length(lw)) {
    np[[tree]] <- tapply(lw[[tree]] * as.numeric(as.character(TRAIN$Y)), nd[[tree]], getmode) #return most frequently occurring class in each node
    m[tree] <- np[[tree]][as.character(ndc[tree])] #get prediction for node with case being predicted
  }
  return (m) 
}

#Function to get probabilities using proportion of trees predicting each class for 3 classes
DefPred3Class <- function(Forest, case, TEST, TRAIN, nd, lw, np){
  ndc <- predict(Forest, newdata = TEST[case,,drop=FALSE], type = "node")
  m <- numeric(length(lw))
  for (tree in 1:length(lw)) {
    #return most frequently occuring response in each node
    np[[tree]] <- c( tapply(lw[[tree]] * as.numeric(as.character(TRAIN$Y)), nd[[tree]], function(x){mean(x==1)/mean(x!=0)}))
    m1 <- np[[tree]][as.character(ndc[tree])]  #tree prediction for a specific case
    np[[tree]] <- c( tapply(lw[[tree]] * as.numeric(as.character(TRAIN$Y)), nd[[tree]], function(x)(mean(x==2)/mean(x!=0))))
    m2 <- np[[tree]][as.character(ndc[tree])]  #tree prediction for a specific case
    np[[tree]] <- c( tapply(lw[[tree]] * as.numeric(as.character(TRAIN$Y)), nd[[tree]], function(x)(mean(x==3)/mean(x!=0))))
    m3 <- np[[tree]][as.character(ndc[tree])]  #tree prediction for a specific case
    #if prop. in node exceeds 0.5, count as 1, if =0.5, flip coin, otherwise count as 0.
    m[tree] <- getwhichmode(c(m1, m2, m3)) #get prediction for node with test case
  }
  m <- c(m, c(1,2,3)) #hack to make sure each class shows up in table
  tab <- table(factor(m))  # counts of trees predicting each case
  return (tab-1) #subtract off 1's that were added
}

#Get class probabilities for each tree
CalcTreeProbs <- function(Forest, case, TEST, TRAIN, nd, lw, np, TreeMC){
  ndc <- predict(Forest, newdata = TEST[case,,drop=FALSE], type = "node")
  m1 <- numeric(length(lw))
  m2 <- numeric(length(lw))
  m3 <- numeric(length(lw))
  for (tree in 1:length(lw)) {
    np[[tree]] <- c( tapply(lw[[tree]] * as.numeric(as.character(TRAIN$Y)), nd[[tree]], function(x){mean(x==1)/mean(x!=0)}))
    m1[tree] <- np[[tree]][as.character(ndc[tree])]  #tree prediction for a specific case
    np[[tree]] <- c( tapply(lw[[tree]] * as.numeric(as.character(TRAIN$Y)), nd[[tree]], function(x)(mean(x==2)/mean(x!=0))))
    m2[tree] <- np[[tree]][as.character(ndc[tree])]  #tree prediction for a specific case
    np[[tree]] <- c( tapply(lw[[tree]] * as.numeric(as.character(TRAIN$Y)), nd[[tree]], function(x)(mean(x==3)/mean(x!=0))))
    m3[tree] <- np[[tree]][as.character(ndc[tree])]  #tree prediction for a specific case
  }
  return(list(m1, m2, m3)) #return weighted proportions for each class
}

#Weighted class probs by OOB accuracy
WtPredsOOBAcc <- function(TreeMC, case, lw, TreeProbs){
    Class1prob <- sum((1-TreeMC)*TreeProbs[case,][[1]])/sum((1-TreeMC))
    Class2prob <- sum((1-TreeMC)*TreeProbs[case,][[2]])/sum((1-TreeMC))
    Class3prob <- sum((1-TreeMC)*TreeProbs[case,][[3]])/sum((1-TreeMC))
    return(c(Class1prob, Class2prob, Class3prob))
    }

#calculate Gini impurity for each node
WtPredsGiniPurity <- function(Forest, case, TRAIN, TEST,  nd, lw, np){
    ndc <- predict(Forest, newdata = TEST[case,,drop=FALSE], type = "node")
    Gini <- numeric(length(lw))
    for (tree in 1:length(lw)) {
      p1 <- c( tapply(lw[[tree]] * as.numeric(as.character(TRAIN$Y)), nd[[tree]], function(x){mean(x==1)/mean(x!=0)}))
      p2 <- c( tapply(lw[[tree]] * as.numeric(as.character(TRAIN$Y)), nd[[tree]], function(x)(mean(x==2)/mean(x!=0))))
      p3 <- c( tapply(lw[[tree]] * as.numeric(as.character(TRAIN$Y)), nd[[tree]], function(x)(mean(x==3)/mean(x!=0))))
      np[[tree]] <- 1-(p1^2+p2^2+p3^2)
      Gini[tree] <- np[[tree]][as.character(ndc[tree])]  #tree prediction for a specific case
    }
    return(Gini) #return weighted proportions for each class
}


WtPredsGiniPurityOOB <- function(Forest, case, TRAIN, nd, lw, np, OOBcases){
  ndc <- predict(Forest, newdata = TRAIN[case,,drop=FALSE], type = "node")
  Gini <- rep(1,length(lw)) #start impurity at 1. Will change for trees where case is OOB
  for (tree in which(OOBcases[,case]==1)) {
    p1 <- c( tapply(lw[[tree]] * as.numeric(as.character(TRAIN$Y)), nd[[tree]], function(x){mean(x==1)/mean(x!=0)}))
    p2 <- c( tapply(lw[[tree]] * as.numeric(as.character(TRAIN$Y)), nd[[tree]], function(x)(mean(x==2)/mean(x!=0))))
    p3 <- c( tapply(lw[[tree]] * as.numeric(as.character(TRAIN$Y)), nd[[tree]], function(x)(mean(x==3)/mean(x!=0))))
    np[[tree]] <- 1-(p1^2+p2^2+p3^2)
    Gini[tree] <- np[[tree]][as.character(ndc[tree])]  #tree prediction for a specific case
  }
  return(Gini) #return weighted proportions for each class
}
  
#Weighted class probs by OOB accuracy
WtPredsGini <- function(case, lw, GiniWts, TreeProbs){
  Class1prob <- sum((1-GiniWts[,case])*TreeProbs[case,][[1]])/sum((1-GiniWts[,case]))
  Class2prob <- sum((1-GiniWts[,case])*TreeProbs[case,][[2]])/sum((1-GiniWts[,case]))
  Class3prob <- sum((1-GiniWts[,case])*TreeProbs[case,][[3]])/sum((1-GiniWts[,case]))
  return(c(Class1prob, Class2prob, Class3prob))
}



#functon to take in test case, make prediction where each tree casts a 0-1 vote
DefPredOOB3Class <- function(Forest, case, TEST, TRAIN, nd, lw, np, OOBcases){
  ndc <- predict(Forest, newdata = TRAIN[case,,drop=FALSE], type = "node")
  m <- numeric(length(lw))
  for (tree in which(OOBcases[,case]==1)) { #only look at trees where case is OOB
    #return most frequently occuring response in each node
    np[[tree]] <- c( tapply(lw[[tree]] * as.numeric(as.character(TRAIN$Y)), nd[[tree]], function(x){mean(x==1)/mean(x!=0)}))
    m1 <- np[[tree]][as.character(ndc[tree])]  #tree prediction for a specific case
    np[[tree]] <- c( tapply(lw[[tree]] * as.numeric(as.character(TRAIN$Y)), nd[[tree]], function(x)(mean(x==2)/mean(x!=0))))
    m2 <- np[[tree]][as.character(ndc[tree])]  #tree prediction for a specific case
    np[[tree]] <- c( tapply(lw[[tree]] * as.numeric(as.character(TRAIN$Y)), nd[[tree]], function(x)(mean(x==3)/mean(x!=0))))
    m3 <- np[[tree]][as.character(ndc[tree])]  #tree prediction for a specific case
    #if prop. in node exceeds 0.5, count as 1, if =0.5, flip coin, otherwise count as 0.
    m[tree] <- getwhichmode(c(m1, m2, m3)) #get prediction for node with test case
  }
  m <- c(m, c(1,2,3)) #hack to make sure each class shows up in table
  tab <- table(factor(m))[2:4]  # counts of trees predicting each case
  return (tab-1) #subtract off 1's that were added
}

CalculatePredictionsSim3Class <- function(size){
  DATA <- Generate3ClassData(size=size)
  TRAIN <- DATA[1:(nrow(DATA)/2),]
  TEST <- DATA[(nrow(DATA)/2+1):(nrow(DATA)),]
  Res <- array(dim=c(length(msvec), 6, nrow(TEST),3)) #nodesize by prediction by test case by class
  OOBRes <- array(dim=c(length(msvec), 6, nrow(TRAIN),3)) #nodesize by prediction by training case by class
  for (ms in 1:length(msvec)){
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
  }
  return(list(OOBRes, Res))
}  

#generate data for simple simulation
Generate2ClassData <- function(size){ #size in number of training and test cases combined
  b0=-2.564
  b1=1
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
  return(DATA)
}

CalculatePredictions2ClassSim <- function(size){
  DATA <- Generate2ClassData(size=size)
  TRAIN <- DATA[1:(nrow(DATA)/2),]
  TEST <- DATA[(nrow(DATA)/2+1):(nrow(DATA)),]
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
  return(list(OOBRes, Res))
}  


#function to calculate variance, exculding OOB cases
CalcVar <- function(Preds, Inbag){
return(var(Preds[Inbag==1]))  
}


# For a regression problem, weight trees by variance in terminal node
WtbyVarReg <- function(Forest, case, TEST, TRAIN, nd, lw, np){
  ndc <- predict(Forest, newdata = TEST[case,,drop=FALSE], type = "node")
  m <- numeric(length(lw))
  v <- numeric(length(lw))
  for (tree in 1:length(lw)) {
    sumx <- tapply(lw[[tree]] * TRAIN$Y, nd[[tree]], sum)  #sum of responses in each node
    n <- tapply(lw[[tree]], nd[[tree]], sum) #observations in each node
   sumxsq <- tapply((lw[[tree]] * TRAIN$Y)^2, nd[[tree]], sum) #sum of squared obs. in each node
   nv <- (sumxsq-(sumx)^2/n)/(n) #variance in each node-need to divide by n, not n-1 since some trees have only 1 obs
    pred <- tapply(lw[[tree]] * TRAIN$Y, nd[[tree]], sum)/ tapply(lw[[tree]], nd[[tree]], sum) #tree prediction for a specific case
   m[tree] <- pred[as.character(ndc[tree])]  #variance in node
   v[tree] <- nv[as.character(ndc[tree])]  #variance in node
  }
  v <- v/sum(v)
  Wtm <- sum(m*(1-v))/sum(1-v)  #weighted mean of tree predictions with weights determined by variance in terminal nodes
  return(mean(Wtm))  
}

WtbyVarOOBReg <- function(Forest, case, TEST, TRAIN, nd, lw, np, OOBcases){
  ndc <- predict(Forest, newdata = TRAIN[case,,drop=FALSE], type = "node")
  m <- numeric(length(lw))
  v <- numeric(length(lw))
  for (tree in which(OOBcases[,case]==1)) { #only look at trees where case is OOB
    np[[tree]] <- tapply(lw[[tree]] * TRAIN$Y, nd[[tree]], sum) /  #prediction associated with each node
      tapply(lw[[tree]], nd[[tree]], sum)
    ones <- tapply(lw[[tree]] * TRAIN$Y, nd[[tree]], sum)  #zeros in each node
    zeros <- tapply(lw[[tree]], nd[[tree]], sum)  - ones # 1's in each node
    nv <- (np[[tree]]^2*zeros+(1-np[[tree]])^2*ones) / (zeros + ones)  #variance in each node
    m[tree] <- np[[tree]][as.character(ndc[tree])]  #tree prediction for a specific case
    v[tree] <- nv[as.character(ndc[tree])]  #variance in node
  }
  wt <- (1-v)*OOBcases[,case]
  Wtm <- sum(m*wt)/sum(wt)  #weighted mean of tree predictions with weights determined by variance in terminal nodes
  return(Wtm)  
}
