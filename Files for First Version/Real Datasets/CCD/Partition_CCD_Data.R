#Function to partition credit card dataset into 10 folds for cross validation
#can be done for more than 1 rep of cross validation, but we only did 1 rep. with 10 folds

CCD=read.csv("CCD.csv")

DATA=CCD
nreps=1
nfolds=10

#array to store the indices of training cases for each rep and fold
#dimensions are (rep, fold, training_indices)
CVTRAINind=array(NA, dim=c(nreps,nfolds,nrow(DATA)-nrow(DATA)/nfolds))
#array to store the indices of test cases for each rep and fold
#dimensions are (rep, fold, test_indices)
CVTESTind=array(NA, dim=c(nreps,nfolds,nrow(DATA)/nfolds))


set.seed(02042017)  #Important to keep seed same for all files, so we're dealing with same datasets
for(rep in 1:nreps){
  orderedcases=sample(1:nrow(DATA),replace=F) #order all cases randomly
  foldsize=nrow(DATA)/nfolds 
  #loop through folds
  for(fold in 1:nfolds){ 
    #take cases in range ((fold-1)*foldsize+1):(fold*foldsize) as test cases and others as training cases
    #ex. first fold has cases 1:3000 as test cases, second has 3001:6000, etc.
    CVTRAINind[rep,fold,]=orderedcases[-orderedcases[((fold-1)*foldsize+1):(fold*foldsize)]]
    CVTESTind[rep,fold,]=orderedcases[orderedcases[((fold-1)*foldsize+1):(fold*foldsize)]]
  }    
}

#save(CVTRAINind, CVTESTind, file="CCDCVpartitions.Rdata")
