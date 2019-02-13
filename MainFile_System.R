# demo file
library(matrixStats)
library(pracma)
library(R.matlab)
library(TunePareto)
rm(list=ls())
old_dir<-setwd("./")
getwd()
## Read the training samples file
data<-read.csv(file.choose())
featurenames=names(data)
newdata<-data.matrix(data)
label=newdata[,length(featurenames)]
ndata<-newdata[,c(4,5)]

## Read the data from mat format
# data<-readMat(file.choose())
#  label=data$Data[,ncol(data$Data)]
#  ndata<-data$Data[,-ncol(data$Data)]
# newdata<-data.matrix(data)
# featurenames=names(data)
#
 nclass<-max(max(label));Nsamples<-0
 for (i in 1:nclass)
 {
   Nsamples[i]<-sum(label==i)
 }
nr=nrow(data);nc=ncol(data)
#attach(data)  # to enable to use the variable names in the file (header)
# Read the testing samples file
#Testdata<-read.csv(file,choose(), header=TRUE)
## conversion into MSnSet Class data



orglabel<-label
 
source('bh_normalize.R')
strt<-Sys.time()
ndata<-bh_normalize(ndata)
print(Sys.time()-strt)
# Random subset the samples
# nsamples=length(label);
# randperum=sample(nsamples);
# ntrain=round(nsamples*0.8)
# subsetindex=randperum[1:ntrain];
# newdata<-ndata[randperum,]
# 
source('bh_randomsubsetsamples.R')

NPer= 0.6 #seq(0.2,0.9, by=0.1)
MeanAccuracy<-list();StdAccuracy<-list()

for (outer in 1:length(NPer))
{
  Acc<-numeric(0)
  SVMAcc<-numeric(0)
  DTAcc<-numeric(0)
  RFAcc<-numeric(0)
  #
  PerTurbo<-list();SVM<-list();DT<-list();RF<-list();
  PerTurboDV<-list();SVMDV<-list();DTDV<-list();RFDV<-list()
  PerTurboClassifiedlabel<-list();SVMClassifiedlabel<-list();DTClassifiedlabel<-list()
  RFClassifiedlabel<-list();
  PerTurboTrainingParameter<-list();SVMTrainingParameter<-list();DTTrainingParameter<-list()
  RFTrainingParameter<-list()
  PerTurbo$OA<-numeric(0);SVM$OA<-numeric(0);DT$OA<-numeric(0);RF$OA<-numeric(0)
for (loop in 1)#:10)  # loop for number of folds
  {
#sss<-bh_randomsubsetsamples(orglabel,"Percentage",NPer[outer])
 sss<-bh_randomsubsetsamples(orglabel,"No_of_samples",100)
TrainData<-ndata[sss$tr_index,];train_label<-orglabel[sss$tr_index];
TestData<-ndata[-sss$tr_index,];testlabel<-orglabel[-sss$tr_index]

# PerTurbo Classification
# Pertubro CV
fold<-5
source('bh_perturbocrossvalidation.R')
str<-Sys.time()
perparams<-bh_perturbocrossvalidation(train_label,TrainData,fold)
print(Sys.time()-str)
#training
source('bh_perturboclassification.R')
option<-"train"
PW<-bh_perturboclassification(TrainData,train_label,option,perparams)
#testing
option<-"test"
PerTurboPrediction<-bh_perturboclassification(TestData,PW,option)
# Classification Results
PerTurboDV[[loop]]<-PerTurboPrediction$Probvalues
PerTurboClassifiedlabel[[loop]]<-PerTurboPrediction$classifiedlabel
PerTurboTrainingParameter[[loop]]<-perparams


nclass=length(unique(testlabel))
# confusion matrix
source('bh_confusionmat.R')
PAcc<-bh_confusionmat(testlabel,PerTurboPrediction$classifiedlabel,option="CI")
PerTurbo$OA<-rbind(PerTurbo$OA, PAcc$OA)
PerTurbo$conf[[loop]]<-PAcc$conf
PerTurbo$UA[[loop]]<-PAcc$UA
PerTurbo$PA[[loop]]<-PAcc$PA

## SVM

# SVM parameter optimization
library(e1071)
kertype<-2;fold<-5;
source('bh_ksvmcrossvalidation.R')
parameter<-bh_ksvmcrossvalidation(TrainData,train_label,kertype,fold)
# SVM Training
option<-"train"
source('bh_svmclassification.R')
kertype<-2;
model<-bh_svmclassification(TrainData,train_label,option,kertype,parameter)
# SVM Classification
option<-"test"
source('bh_svmclassification.R')
SVMPrediction<-bh_svmclassification(model,TestData,option)
# Classification Results
SVMDV[[loop]]<-SVMPrediction$Probvalues
SVMClassifiedlabel[[loop]]<-SVMPrediction$classifiedlabel
SVMTrainingParameter[[loop]]<-model

# SVM Acc
source('bh_confusionmat.R')
SAcc<-bh_confusionmat(testlabel,SVMPrediction$classifiedlabel,option="CI")
SVMAcc[loop]=SAcc$OA[1]
SVM$OA<-rbind(SVM$OA, SAcc$OA) 
SVM$conf[[loop]]<-SAcc$conf
SVM$UA[[loop]]<-SAcc$UA
SVM$PA[[loop]]<-SAcc$PA

## Decision Tree
source('bh_decisiontreeclassification.R')
option="train"
DTmodel<-bh_decisiontreeclassification(TrainData,train_label,option)

DTPrediction<-bh_decisiontreeclassification(DTmodel,TestData,option="test")
# Classification Results
DTDV[[loop]]<-DTPrediction$Probvalues
DTClassifiedlabel[[loop]]<-DTPrediction$classifiedlabel
DTTrainingParameter[[loop]]<-DTmodel
# confusion matrix
DAcc<-bh_confusionmat(testlabel,DTPrediction$classifiedlabel,option="CI")


DTAcc[loop]<-DAcc$OA[1]

DT$OA<-rbind(DT$OA, DAcc$OA) 
DT$conf[[loop]]<-DAcc$conf
DT$UA[[loop]]<-DAcc$UA
DT$PA[[loop]]<-DAcc$PA

## Random Forest
source('bh_randomforest.R')
option="train"
RFmodel<-bh_randomforest(TrainData,train_label,option)

RFPrediction<-bh_randomforest(RFmodel,TestData,option="test")
# Classification results
RFDV[[loop]]<-RFPrediction$Probvalues
RFClassifiedlabel[[loop]]<-RFPrediction$classifiedlabel
RFTrainingParameter[[loop]]<-RFmodel
#confusion matrix
RAcc<-bh_confusionmat(testlabel,RFPrediction$classifiedlabel,option="CI")
RFAcc[loop]<-RAcc$OA[1]

RF$OA<-rbind(RF$OA, RAcc$OA) 
RF$conf[[loop]]<-RAcc$conf
RF$UA[[loop]]<-RAcc$UA
RF$PA[[loop]]<-RAcc$PA

## MCS combination function
}
## Accuracy write in Excel
  setwd("D:/PostDocWork/Denmark/Codes")
  source('bh_confwriteexcel.R')
# PerTurbo
  trp<-as.character(NPer[outer]);testp<-as.character(1-NPer[outer])
  fname=paste("ISPRS7_PerTurbo_tr",trp,"_test",testp,".xlsx", sep="")
#fname="ISPRS_PerTurbo_90tr_10test.xlsx"
option<-1
bh_confwriteexcel(PerTurbo, fname,option)
  # SVM
fname=paste("ISPRS7_SVM_tr",trp,"_test",testp,".xlsx", sep="")
#fname="ISPRS7_SVM_10tr_90test.xlsx"
option<-2
setwd(old_dir)
bh_confwriteexcel(SVM, fname,option) 
# Decision Tree
fname=paste("ISPRS7_DT_tr",trp,"_test",testp,".xlsx", sep="")
#fname="ISPRS7_DT_10tr_90test.xlsx"
option<-3
setwd(old_dir)
bh_confwriteexcel(DT, fname,option) 
# Random Forest
fname=paste("ISPRS7_RF_tr",trp,"_test",testp,".xlsx", sep="")
#fname="ISPRS7_RF_10tr_90test.xlsx"
option<-4
setwd(old_dir)
bh_confwriteexcel(RF, fname,option) 
  ########### ++++++++++++++++++++++++++++++++++++++++++++#################
MeanAccuracy$PerTurbo[[outer]]<-colMeans(PerTurbo$OA)
StdAccuracy$PerTurbo[[outer]]<-sqrt(colVars(PerTurbo$OA))
MeanAccuracy$SVM[[outer]]<-colMeans(SVM$OA)
StdAccuracy$SVM[[outer]]<-sqrt(colVars(SVM$OA))
MeanAccuracy$DT[[outer]]<-colMeans(DT$OA)
StdAccuracy$DT[[outer]]<-sqrt(colVars(DT$OA))

setwd("D:/PostDocWork/Denmark/Codes/Results")
save(PerTurbo,file=paste("PerTurbo_Results_Percent_",as.character(NPer[outer]),sep=""))
save(SVM,file=paste("SVM_Results_Percent_",as.character(NPer[outer]),sep=""))
save(DT,file=paste("DT_Results_Percent_",as.character(NPer[outer]),sep=""))
setwd("D:/PostDocWork/Denmark/Codes")

######## ++++++ Decison values and labels write
setwd("D:/PostDocWork/Denmark/Codes/Results")
# Decision values
save(PerTurboDV, SVMDV, DTDV, RFDV, file=paste("ISPRS_Classifiers_DecisionValue_", as.character(NPer[outer]), sep=""))
# Classifiedlabels
save(PerTurboClassifiedlabel, SVMClassifiedlabel, DTClassifiedlabel, RFClassifiedlabel, 
     file=paste("ISPRS_Classifiers_Classifiedlabel_", as.character(NPer[outer]), sep=""))
}
setwd("D:/PostDocWork/Denmark/Codes/Results")
TrainingPercent<-NPer;
save(MeanAccuracy, StdAccuracy,TrainingPercent,file="MeanAccuarcy_Measures_ofclassifier")




 