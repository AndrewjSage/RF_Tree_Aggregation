#Since calculations were run separately, we pull together results from different .Rdata files
library(abind)

#"STEMRes_Init1.Rdata" contains results for first nodesize setting and first 7 folds
#We load these one at a time and combine them into a single large file 
#Indices on OOBPreds and TestPreds are
#1-rep #2-fold #3-Pred Method #4-Case #5-mtry #6-nodesize #7-minbucket
load("STEMRes_Init1.Rdata")
OOBPredictions=OOBPreds
Predictions=TestPreds
load("STEMRes_Init2.Rdata")
Predictions=abind(Predictions, TestPreds, along=6)
OOBPredictions=abind(OOBPredictions, OOBPreds, along=6)
load("STEMRes_Init3.Rdata")
Predictions=abind(Predictions, TestPreds, along=6)
OOBPredictions=abind(OOBPredictions, OOBPreds, along=6)
load("STEMRes_Init4.Rdata")
Predictions=abind(Predictions, TestPreds, along=6)
OOBPredictions=abind(OOBPredictions, OOBPreds, along=6)
load("STEMRes_Init5.Rdata")
Predictions=abind(Predictions, TestPreds, along=6)
OOBPredictions=abind(OOBPredictions, OOBPreds, along=6)
load("STEMRes_Init6.Rdata")
Predictions=abind(Predictions, TestPreds, along=6)
OOBPredictions=abind(OOBPredictions, OOBPreds, along=6)
load("STEMRes_Init7.Rdata")
Predictions=abind(Predictions, TestPreds, along=6)
OOBPredictions=abind(OOBPredictions, OOBPreds, along=6)
load("STEMRes_Init8.Rdata")
Predictions=abind(Predictions, TestPreds, along=6)
OOBPredictions=abind(OOBPredictions, OOBPreds, along=6)

save(Predictions, OOBPredictions, file="Predictions1.Rdata")

load("STEMRes_Init9.Rdata")
OOBPredictions=OOBPreds
Predictions=TestPreds
load("STEMRes_Init10.Rdata")
Predictions=abind(Predictions, TestPreds, along=6)
OOBPredictions=abind(OOBPredictions, OOBPreds, along=6)
load("STEMRes_Init11.Rdata")
Predictions=abind(Predictions, TestPreds, along=6)
OOBPredictions=abind(OOBPredictions, OOBPreds, along=6)
load("STEMRes_Init12.Rdata")
Predictions=abind(Predictions, TestPreds, along=6)
OOBPredictions=abind(OOBPredictions, OOBPreds, along=6)
load("STEMRes_Init13.Rdata")
Predictions=abind(Predictions, TestPreds, along=6)
OOBPredictions=abind(OOBPredictions, OOBPreds, along=6)
load("STEMRes_Init14.Rdata")
Predictions=abind(Predictions, TestPreds, along=6)
OOBPredictions=abind(OOBPredictions, OOBPreds, along=6)
load("STEMRes_Init15.Rdata")
Predictions=abind(Predictions, TestPreds, along=6)
OOBPredictions=abind(OOBPredictions, OOBPreds, along=6)
load("STEMRes_Init16.Rdata")
Predictions=abind(Predictions, TestPreds, along=6)
OOBPredictions=abind(OOBPredictions, OOBPreds, along=6)

save(Predictions, OOBPredictions, file="Predictions2.Rdata")

load("Predictions1_1_7.Rdata")
TestPredictions=Predictions
load("Predictions2.Rdata")
TestPredictions=abind(TestPredictions, Predictions, along=6)
save(TestPredictions,  file="Combined_Test_Predictions_1_7.Rdata")
