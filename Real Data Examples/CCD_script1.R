setwd("/work/STAT/ajsage")
source("Aggregation_Functions.R")
library(parallel)

DATA <- read.csv("CCD.csv", header=TRUE)[,-1]
msvec <- c(1)
nfolds=10


set.seed(1252018)
Folds <- matrix(unlist(sample(1:nrow(DATA))), ncol=nfolds)

#Apply iteratively using parSapply

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)
clusterExport(cl, list("DATA", "msvec", "nfolds", "Folds", "DefPred2Class",
                       "DefPredOOB2Class","getmode", "DefPred3Class",
                       "WtbyVar2Class","WtbyVarOOB2Class", "GetOOBPred",
                       "WtbyOOBPerf", "CalculatePredictions"))
clusterEvalQ(cl, {
  library("partykit")
})

clusterSetRNGStream(cl, 03142018)
Res_CCD <- parSapply(cl=cl, X=1:10, FUN=CalculatePredictions)
stopCluster(cl)

save(Res_CCD, file="Res_CCD1.Rdata")