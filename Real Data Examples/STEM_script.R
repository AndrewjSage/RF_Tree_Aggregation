setwd("/work/STAT/ajsage")
source("Aggregation_Functions.R")
library(parallel)

DATA <- read.csv("STEM.csv", header=TRUE)[,-1]
DATA$LC_type <- as.numeric(DATA$LC_type != 0)  #Yes/No for LC
names(DATA)[length(names(DATA))] <- "Y"

msvec <- c(1,5,20,100,300,500)
nfolds=9
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
Res_STEM <- parSapply(cl=cl, X=1:nfolds, FUN=CalculatePredictions)
stopCluster(cl)

save(Res_STEM, file="Res_STEM.Rdata")