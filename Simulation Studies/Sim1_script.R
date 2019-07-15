setwd("/work/STAT/ajsage")
source("Aggregation_Functions.R")
library(parallel)

msvec <- c(1,5,20,50,100,200)
set.seed(1252018)

#Apply iteratively using parSapply

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)
clusterExport(cl, list("msvec","DefPred2Class",
                       "DefPredOOB2Class","getmode", "getwhichmode", "DefPred3Class",
                       "WtbyVar2Class","WtbyVarOOB2Class", "GetOOBPred",
                       "WtbyOOBPerf", "CalculatePredictions", "Generate3ClassData", "GetOOBPred3Class",
                       "DefPred3Class", "CalcTreeProbs", "WtPredsOOBAcc", "WtPredsGiniPurity", "WtPredsGiniPurityOOB",
                       "WtPredsGini", "DefPredOOB3Class", "CalculatePredictionsSim3Class", "sampfunc", 
                       "CalculatePredictions2ClassSim", "Generate2ClassData"))
clusterEvalQ(cl, {
  library("partykit")
})

clusterSetRNGStream(cl, 03142018)
Res_Sim1 <- parSapply(cl=cl, X=1:100, FUN=function(i){CalculatePredictions2ClassSim(size=2000)})
stopCluster(cl)

save(Res_Sim1, file="Res_Sim1.Rdata")