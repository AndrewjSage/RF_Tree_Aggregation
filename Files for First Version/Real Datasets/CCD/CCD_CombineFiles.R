#Since calculations were run separately, we pull together results from different .Rdata files
library(abind)

#First Simulation ()
load("CCDRes1.Rdata")
#Indices on OOBPreds and TestPreds are
#1-rep #2-fold #3-Pred Method #4-Case #5-mtry #6-nodesize #7-minbucket
OOBPredictions1=OOBPreds
Predictions1=TestPreds
#Need to combine with data from 10th fold which was run separetely
load("CCDRes1f10.Rdata")
OOBPredictions1[,10,,,,,]=OOBPreds[,10,,,,,]
Predictions1[,10,,,,,]=TestPreds[,10,,,,,]


#Second Simulation
load("CCDRes2.Rdata")
#Indices on OOBPreds and TestPreds are
#1-rep #2-fold #3-Pred Method #4-Case #5-mtry #6-nodesize #7-minbucket
OOBPredictions2=OOBPreds
Predictions2=TestPreds

#Third Simulation
load("CCDRes3.Rdata")
#Indices on OOBPreds and TestPreds are
#1-rep #2-fold #3-Pred Method #4-Case #5-mtry #6-nodesize #7-minbucket
OOBPredictions3=OOBPreds
Predictions3=TestPreds

#Fourth Simulation ()
load("CCDRes4.Rdata")
#Indices on OOBPreds and TestPreds are
#1-rep #2-fold #3-Pred Method #4-Case #5-mtry #6-nodesize #7-minbucket
OOBPredictions4=OOBPreds
Predictions4=TestPreds
#Need to combine with data from 10th fold which was run separetely
load("CCDRes4f10.Rdata")
OOBPredictions4[,10,,,,,]=OOBPreds[,10,,,,,]
Predictions4[,10,,,,,]=TestPreds[,10,,,,,]

#Fifth Simulation ()
load("CCDRes5.Rdata")
#Indices on OOBPreds and TestPreds are
#1-rep #2-fold #3-Pred Method #4-Case #5-mtry #6-nodesize #7-minbucket
OOBPredictions5=OOBPreds
Predictions5=TestPreds
#Need to combine with data from 10th fold which was run separetely
load("CCDRes5f10.Rdata")
OOBPredictions5[,10,,,,,]=OOBPreds[,10,,,,,]
Predictions5[,10,,,,,]=TestPreds[,10,,,,,]


#Sixth Simulation ()
load("CCDRes6.Rdata")
#Indices on OOBPreds and TestPreds are
#1-rep #2-fold #3-Pred Method #4-Case #5-mtry #6-nodesize #7-minbucket
OOBPredictions6=OOBPreds
Predictions6=TestPreds

#Need to arrange with the nodesizes in the right order since they are scrambled in original simulations

#Nodesize1-simulation 1
#Nodesize10-simulation 2
#Nodesize25-simulation 3
#Nodesize50-simulation 4
#Nodesize100-simulation 5
#Nodesize200-simulation 6
#Nodesize300-simulation 6
#Nodesize500-simulation 5
#Nodesize1000-simulation 4
#Nodesize5000-simulation 3

#along=-1 creates new dim for nodesize. Now nodesize is first
Predictions=abind(Predictions1[,,,,,1,], Predictions2[,,,,,1,], Predictions3[,,,,,1,], Predictions4[,,,,,1,], Predictions5[,,,,,1,], Predictions6[,,,,,1,],Predictions6[,,,,,2,],Predictions5[,,,,,2,],Predictions4[,,,,,2,],Predictions3[,,,,,2,],along=-1)
OOBPredictions=abind(OOBPredictions1[,,,,,1,], OOBPredictions2[,,,,,1,], OOBPredictions3[,,,,,1,], OOBPredictions4[,,,,,1,], OOBPredictions5[,,,,,1,], OOBPredictions6[,,,,,1,],OOBPredictions6[,,,,,2,],OOBPredictions5[,,,,,2,],OOBPredictions4[,,,,,2,],OOBPredictions3[,,,,,2,], along=-10)
#1-nodesize, #2=fold #3=pred. method, #4=case #5=mtry

save(Predictions, OOBPredictions, file="Preds.Rdata")

