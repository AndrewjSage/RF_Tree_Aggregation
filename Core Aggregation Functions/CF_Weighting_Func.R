#This script contains functions to obtain probability estimates that would result from
#the randomForest aggregation approach that weights each tree equally, applied to a cForest 
#object. The UnweightCF and UnweightCFOOB functions are the main routines. Use UnweightCF 
#for predictions on new test cases, and UnweightCFOOB for out-of-bag predictions
#on training data. 

#Helper Function- Provide summary of terminal nodes for a given tree in forest
#Takes in the CF object, the tree number, and a dataframe with training data
TreeSummary=function(forest, Treenum, TRAIN){  #forest is name of Conditional inference forest, Treenum is #trees, TRAIN is name of training set
  #CF@where[[1]] gives the numbers of the terminal node each test case lands in for tree 1 0=case OOB in tree 1
  CF=forest
  df=data.frame(1:nrow(TRAIN), CF@where[[Treenum]], TRAIN$Class)    #Find terminal node that each training case landed in. Node 0 is OOB cases
  names(df)=c("ID", "Node", "Class")
  TreeSummary=ddply(df,.(Node), summarise, n_Train=length(unique(ID)), C0=mean(as.numeric(as.character(Class))==0),C1=mean(as.numeric(as.character(Class))==1),C2=mean(as.numeric(as.character(Class))==2))
  TreeSummary=subset(TreeSummary, Node!=0) #eliminate Node 0 (OOB cases that weren't used)
  TreeSummary$Tree=Treenum
  TreeSummary=TreeSummary[order(-TreeSummary$n_Train),]
  return(TreeSummary)
}

#Similar tree summary function to be used when computing out-of-bag estimates.
TreeSummaryOOB=function(forest, Treenum, TRAIN){  #forest is name of Conditional inference forest, Treenum is #trees, TRAIN is name of training set
  #CF@where[[1]] gives the numbers of the terminal node each test case lands in for tree 1 0=case OOB in tree 1
  CF=forest
  df=data.frame(1:nrow(TRAIN), CF@where[[Treenum]], TRAIN$Class)    #Find terminal node that each training case landed in. In older version of party, Node 0 is OOB cases. Now we need to do this manually.
  df[,2][CF@weights[[Treenum]]==0]=0 #Set OOB cases to node 0 This step is not in original TreeSummary function.
  names(df)=c("ID", "Node", "Class")
  TreeSummary=ddply(df,.(Node), summarise, n_Train=length(unique(ID)), C0=mean(as.numeric(as.character(Class))==0),C1=mean(as.numeric(as.character(Class))==1),C2=mean(as.numeric(as.character(Class))==2))
  TreeSummary=subset(TreeSummary, Node!=0) #eliminate Node 0 (OOB cases that weren't used)
  TreeSummary$Tree=Treenum
  TreeSummary=TreeSummary[order(-TreeSummary$n_Train),]
  return(TreeSummary)
}


#########################

#Main Function for obtaining probability estimates for new cases
#Takes in an object from CF, dataframes for training and test data, and number of trees in CF
#Outputs dataframe with one row per test case. Columns are estimated probabilities for 
#each class, using equal weighting of trees (as in RF-regression)
#Can handle up to 3 classes, although works for 2. 

UnweightCF=function(CF, TRAIN, TEST, ntrees){
  #Dataframe with all trees together
  All_Trees=NA
  for (Treenum in 1:ntrees){
    All_Trees=rbind(All_Trees, TreeSummary(CF,Treenum, TRAIN))
  }
  All_Trees=subset(All_Trees, C0!="NA")
  #This is a trick to make sure that no node indices are skipped, which would mess up the matrix
  Extra_Nodes=data.frame(matrix(c(1:max(All_Trees$Node),rep(NA,max(All_Trees$Node)*(ncol(All_Trees)-1))) , nrow=max(All_Trees$Node), byrow=F))
  names(Extra_Nodes)=names(All_Trees)
  All_Trees=rbind(All_Trees, Extra_Nodes)
  
  #Convert to matrix
  #Matrix[Tree, Node] gives probability of being in a class based on training cases in that node.
  Tree_Prbs=array(NA)
  library(reshape2)
  M_C0=acast(All_Trees, Tree~Node, value.var ="C0") #probabilities for staying in STEM at ISU
  M_C1=acast(All_Trees, Tree~Node, value.var ="C1") #probabilities for leaving STEM at ISU
  M_C2=acast(All_Trees, Tree~Node, value.var ="C2") #probabilties for leaving ISU
  Tree_Prbs=array(c(M_C0, M_C1, M_C2), dim=c(nrow(M_C1),ncol(M_C1),3))
  Tree_Nodesize_Matrix=acast(All_Trees, Tree~Node, value.var ="n_Train")
  
  #Produce an nxn matrix (n=number of cases in test set) with the terminal node that
  #each case lands in for each tree
  
  #each row indicates a tree
  #each column indicates a case
  Term_Nodes=matrix(NA, ncol=nrow(TEST), nrow=ntrees)
  for (Treenum in 1:ntrees){
    Term_Nodes[Treenum, ]=where(CF, newdata=TEST)[[Treenum]]
  }

#Get from terminal nodes to probabilities
  #First index represents tree
  #Second index represent test case
  #Third index represents class (0=stay, 1=leave stem, 2=leave ISU)
  Case_Tree_Prbs=array(NA, dim=c(ntrees, nrow(TEST), 3 ))
  for (i in 1:nrow(Case_Tree_Prbs)){
    for (j in 1:ncol(Case_Tree_Prbs)){
      Case_Tree_Prbs[i,j,]=Tree_Prbs[i,Term_Nodes[i,j],]
    }
  }    
  CFPrb_uw_C0=colMeans(Case_Tree_Prbs[,,1])
  CFPrb_uw_C1=colMeans(Case_Tree_Prbs[,,2])
  CFPrb_uw_C2=colMeans(Case_Tree_Prbs[,,3])
  CFPrb_uw=matrix(c(CFPrb_uw_C0,CFPrb_uw_C1,CFPrb_uw_C2), ncol=3, byrow=F)
 # names(CFPrb_uw)=c("C0", "C1", "C2")
  return(CFPrb_uw)
}

###############################################################################################

#Main function for OOB predictions
UnweightCFOOB=function(CF, TRAIN, ntrees){
  #Dataframe with all trees together
  All_Trees=NA
  for (Treenum in 1:ntrees){
    All_Trees=rbind(All_Trees, TreeSummaryOOB(CF,Treenum, TRAIN))
  }
  All_Trees=subset(All_Trees, C0!="NA")
  #This is a trick to make sure that no node indices are skipped, which would mess up the matrix
  Extra_Nodes=data.frame(matrix(c(1:max(All_Trees$Node),rep(NA,max(All_Trees$Node)*(ncol(All_Trees)-1))) , nrow=max(All_Trees$Node), byrow=F))
  names(Extra_Nodes)=names(All_Trees)
  All_Trees=rbind(All_Trees, Extra_Nodes)  #dataframe containing all terminal nodes in all trees
  
  #Convert to matrix
  #Matrix[Tree, Node] gives probability of being in a class based on training cases in that node.
  Tree_Prbs=array(NA)
  library(reshape2)
  M_C0=acast(All_Trees, Tree~Node, value.var ="C0") #probabilities for cat 0
  M_C1=acast(All_Trees, Tree~Node, value.var ="C1") #probabilities for cat 1 
  M_C2=acast(All_Trees, Tree~Node, value.var ="C2") #probabilties for cat 2
  Tree_Prbs=array(c(M_C0, M_C1, M_C2), dim=c(nrow(M_C1),ncol(M_C1),3))
  Tree_Nodesize_Matrix=acast(All_Trees, Tree~Node, value.var ="n_Train") #Rows represent trees, columns terminal nodes, and entries are number of cases
  
  #Produce an nxn matrix (n=number of cases in test set) with the terminal node that
  #each case lands in for each tree
  
  #each row indicates a tree
  #each column indicates a test case  
  #This part is different for the OOB cases than it was for predicting new cases
  #Predict training cases in all trees. Then eliminate in-bag cases
  Term_Nodes=matrix(NA, ncol=nrow(TRAIN), nrow=ntrees)
  for (Treenum in 1:ntrees){
    Term_Nodes[Treenum, ]=where(CF, newdata=TRAIN)[[Treenum]]
    Term_Nodes[Treenum,CF@weights[[Treenum]]==1]=NA  #Get rid of inbag cases
  }
  
  #Get from terminal nodes to probabilities
  #First index represents tree
  #Second index represent test case
  #Third index represents class (0=stay, 1=leave stem, 2=leave ISU)
  Case_Tree_Prbs=array(NA, dim=c(ntrees, nrow(TRAIN), 3 ))
  for (i in 1:nrow(Case_Tree_Prbs)){
    for (j in 1:ncol(Case_Tree_Prbs)){
      Case_Tree_Prbs[i,j,]=Tree_Prbs[i,Term_Nodes[i,j],]
    }
  }    

  #Need to get rid of cases that weren't OOB in tree t (already did this.)
  #for (tree in 1:nrow(Case_Tree_Prbs)){
  #  Case_Tree_Prbs[tree,CF@weights[[tree]]!=0,]=NA  #This needed to be changed in party 1.2.3
#  }
  CFPrb_uw_C0=colMeans(Case_Tree_Prbs[,,1], na.rm=T) #Proportion of class 0 in terminal node containing test case
  CFPrb_uw_C1=colMeans(Case_Tree_Prbs[,,2], na.rm=T) #Proportion of class 1 in terminal node containing test case
  CFPrb_uw_C2=colMeans(Case_Tree_Prbs[,,3], na.rm=T) #Proportion of class 2 in terminal node containing test case
  CFPrb_uw=matrix(c(CFPrb_uw_C0,CFPrb_uw_C1,CFPrb_uw_C2), ncol=3, byrow=F)
  # names(CFPrb_uw)=c("C0", "C1", "C2")
  return(CFPrb_uw)
}