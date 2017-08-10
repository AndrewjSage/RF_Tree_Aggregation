##########################################################################
##Table for illustration in simulation study sections

library(plyr)
library(party)

#Generate 150 observations first 100 are meant to be training data, last 50 test data.
#We end up just picking 1 test case to illustrate.
#This is meant only as an illustration of how probabilities can be different, not an attempt to
#say which is better
size=150
set.seed(03292016)
X1=rnorm(size,0,1)
X2=rnorm(size,0,1)
X3=rnorm(size,0,1)
X4=rnorm(size,0,1)
X=c(X1,X2,X3,X4)
dim(X) <- c(size, 4)
#Linear predictor
eta=-2*X1
Class=c(rep(NA),size)
#Simulate 0-1 response
for(i in 1:size){
  Class[i]=rbinom(1,1,exp(eta[i])/(1+exp(eta[i])))
}
#Convert response to binary factor variable
Class=as.factor(Class)
#Vector of true success probabilities
prob=exp(eta)/(1+exp(eta))
#Combine predictors and respons into 1 dataframe
DATA=data.frame(X,Class)

#Separate training data from test data by putting first 2/3 into training data
trainprop=2/3
TRAIN=DATA[1:(size*trainprop), ]
TEST=DATA[(size*trainprop+1):size, ]

#Grow small random forest using cForest
ntrees=20
CF=cforest(Class~., data=TRAIN, controls=cforest_unbiased(ntree=ntrees, mtry=2, minsplit=5, minbucket=0))
CFPred=unlist(predict(CF, newdata=TEST, type="prob")) 
CFPrb=CFPred[seq(2,2*nrow(TEST),2)]  #Success probabilities
#Compute probabilities when trees are weighted equally
source("~/Aggregation Function/CF_Weighting_Func.R")
CFPrb_uw=UnweightCF(CF,TRAIN,TEST,ntrees)[,2]


#Get summaries of each tree for table in paper
#Create summary of tree predictions for 1 test case. We use a case like test case #41, which illustrates
#a strong difference in probability estimates

#Function to get summary of terminal nodes for a given tree. This is only setup to work after
#the preceding code has been run, as names refer to the names used for those objects

#For each node, returns #of training and test cases, proportion of cases in the node that are 
#1's, and proportion of training and test cases that land in that node
NodeSummary=function(Treenum){ #Argument is the number of the tree in the forest
  df=data.frame(1:nrow(TRAIN), CF@where[[Treenum]], TRAIN$Class)
  names(df)=c("ID", "Node", "Class")
  CFTreeSummary=ddply(df,.(Node), summarise, n_Train=length(unique(ID)), Train_Prop_Class=mean(as.numeric(as.character(Class))))
  Testnodes=data.frame(1:nrow(TEST), where(CF, newdata=TEST)[[Treenum]], TEST$Class)
  names(Testnodes)=c("ID", "Node", "Test_Prop_Class")
  Testnodes=ddply(Testnodes,.(Node), summarise, n_Test=length(unique(ID)), Test_Prop_Class=mean(as.numeric(as.character(Test_Prop_Class))))
  TreeSummary=merge(CFTreeSummary, Testnodes, by="Node", all=TRUE)
  TreeSummary$Tree=Treenum
  TreeSummary=subset(TreeSummary, Node!=0) #eliminate Node 0 (OOB cases that weren't used)
  TreeSummary$Ratio_Train_Cases=TreeSummary$n_Train/sum(TreeSummary$n_Train)  #Ratio of train cases that end up in each node
  TreeSummary$Ratio_Test_Cases=TreeSummary$n_Test/sum(TreeSummary$n_Test, na.rm=T) #Ratio of test cases that end up in each node
  return(TreeSummary)
}

#Combine information from all trees
All_Trees=NA
for (Treenum in 1:ntrees){
  All_Trees=rbind(All_Trees, NodeSummary(Treenum))
  All_Trees=subset(All_Trees, Test_Prop_Class!="NA")
}
#This is a trick to make sure that no node indices are skipped, which would mess up the matrix
Extra_Nodes=data.frame(1:max(All_Trees$Node), rep(NA,max(All_Trees$Node)),rep(NA,max(All_Trees$Node)),rep(NA,max(All_Trees$Node)),rep(NA,max(All_Trees$Node)),rep(NA,max(All_Trees$Node)),rep(NA,max(All_Trees$Node)),rep(max(All_Trees$Tree)+1,max(All_Trees$Node)) )
names(Extra_Nodes)=names(All_Trees)
All_Trees=rbind(All_Trees, Extra_Nodes)

#Convert to matrix
#Tree_Prb_Matrix[Tree, Node] gives estimated success probability for each node, tree 
#Tree_Nodesize_Matrix[Tree, Node] gives the number of cases in each node and tree
library(reshape2)
Tree_Prb_Matrix=acast(All_Trees, Tree~Node, value.var ="Train_Prop_Class")
Tree_Nodesize_Matrix=acast(All_Trees, Tree~Node, value.var ="n_Train")
#There are NA's since not every tree has a node with each given number

#Produce an nxn matrix (n=number of cases in test set) with the terminal node that
#each test case lands in for each tree
#each row indicates a tree
#each column indicates a test case
Term_Nodes=matrix(NA, ncol=nrow(TEST), nrow=ntrees)
for (Treenum in 1:ntrees){
  Term_Nodes[Treenum, ]=where(CF, newdata=TEST)[[Treenum]]
}

#Get from terminal nodes to probabilities
#Estimated success probability for each test case according to each tree
Case_Tree_Prbs=matrix(NA, ncol=nrow(TEST), nrow=ntrees)
for (i in 1:nrow(Case_Tree_Prbs)){
  for (j in 1:ncol(Case_Tree_Prbs)){
    Case_Tree_Prbs[i,j]=Tree_Prb_Matrix[i,Term_Nodes[i,j]]
  }
}
#Dimensions are ntree by ntest

#Matrix with number of training cases in terminal node containing each test case
#Dimensions are ntree by ntest
Case_Node_sizes=matrix(NA, ncol=nrow(TEST), nrow=ntrees)
for (i in 1:nrow(Case_Node_sizes)){
  for (j in 1:ncol(Case_Node_sizes)){
    Case_Node_sizes[i,j]=Tree_Nodesize_Matrix[i,Term_Nodes[i,j]]
  }
}

#Find a case that makes for a good illustration of how these probabilities can be different
i=41
Case1=data.frame(1:ntrees)
#Create summary of terminal nodes that this case lands in
names(Case1)=c("Tree")
Case1$Nodesize=Case_Node_sizes[,i]
Case1$Pos=Case1$Nodesize*Case_Tree_Prbs[,i]
Case1$Prob=Case_Tree_Prbs[,i]  
Case1=Case1[order(-Case1$Nodesize),]
Case1
#EW prb
mean(Case1$Prob)
#PW Prb
sum(Case1$Nodesize*Case1$Prob)/sum(Case1$Nodesize)
#True Probability
et=-2*TEST[i,1]
exp(et)/(1+exp(et))
#Check that these match output
CFPrb[41]
CFPrb_uw[41]

#Since this is just an approximation due to rounding, create new case where we specify exact x's
#So we can get an exact probability for the paper.
TEST1=TEST
TEST1[41,]=c(0.6207, 1.8119, 1.9120, -1.3638,0)
CFPred=unlist(predict(CF, newdata=TEST1, type="prob")) 
CFPrb=CFPred[seq(2,2*nrow(TEST1),2)]  #Probabilities of staying in STEM at ISU
source("CF_Weighting_Func.R")
CFPrb_uw=UnweightCF(CF,TRAIN,TEST1,ntrees)[,2]
CFPrb[41]
CFPrb_uw[41]
et=-2*TEST1[i,1]
exp(et)/(1+exp(et))
#Comes out same so just use rounded version.

