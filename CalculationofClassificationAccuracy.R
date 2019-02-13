# Classiifcation Accuarcy calucaltion
rm(list=ls())
source('bh_loadLib.R')
bh_loadLib()
library('irr')
# Load the original label
### ISPRS Dataset
fname<-"D:/PostDocWork/Denmark/AttributeProfiles/ISPRS/ISPRS_7_GT_SampleLocation.mat"
SampleLocationIndex<-readMat(fname)
nam<-names(SampleLocationIndex)
SampleLocationIndex<-SampleLocationIndex[[nam[1]]]
fname<-"D:/PostDocWork/Denmark/AttributeProfiles/ISPRS/ISPRS_7_GT_Label_Classwise.mat"
label<-readMat(fname)
nam<-names(label)
label<-label[[nam[1]]]
##
sc<-sort(SampleLocationIndex, decreasing = FALSE, index.return=TRUE)
nlabel<-label[sc$ix]
#
orglabel<-nlabel
# Load the testing sample location Indexes
fname<-"D:/PostDocWork/Denmark/AttributeProfiles/ClassificationResults/NDVI+NDVIAP_500/ISPRS_7_NDVIAP_TRTestSampleLocationIndex_500"
sss<-load(fname)
############################## rCD Dataset
# fname<-"D:/PostDocWork/Denmark/Datas/RCD/RCD30_TrainingClassImage.mat"
# ClassImage<-readMat(fname)
# nam<-names(ClassImage)
# ClassImage<-ClassImage[[nam[1]]]
# SampleLocationIndex<-which(ClassImage>0)
# nlabel<-ClassImage[SampleLocationIndex]
# orglabel<-nlabel
# ##
# 
# 
# # Loading pre-generated testing and training location indexes
# fname<-"D:/PostDocWork/Denmark/ClassificationResults/RCD_500samples/RCD30_OrgImg_TRTestSampleLocationIndex_500"
# sss<-load(fname)

####################
# Load the classification labels
fname<-"D:/PostDocWork/Denmark/ClassificationResults/ISPRS7_500samples/OrgImg+DSM+NDVI+NDVIAP/ISPRS_SVM_OrgImg_DSM_NDVI_NDVIAP_Classifiedlabel_500"
# fname<-"D:/PostDocWork/Denmark/ClassificationResults/RCD_500samples/OrgImg+DSM+NDVI+NDVI_AP/RCD30_Classifiers_OrgImg_NDVI_NDSM_NDVIAP_Classifiedlabel500"
classifiedlabels<-load(fname)

# read the classified image
# source('bh_classifiedimageread.R')
# classifiedlabels<-bh_classifiedimageread()

# Classiifcation accuarcy
DTAcc<-numeric(0)
RFAcc<-numeric(0)
NBAcc<-numeric(0);MLRAcc<-numeric(0)
DT<-list();RF<-list();NB<-list();MLR<-list()
PerTurbo<-list();SVM<-list()
source('bh_confusionmat.R')
for (fold in 1:10)
{
  ######## ISPRS 7
testlocation_index=TrainTestSampleIndex[[fold]]$test_index;
testlabel<-orglabel[testlocation_index]
#### RCD
#   testlocation_index<-SampleLocationIndex[TrainTestSampleIndex[[loop]]$test_index];
# testlabel<-orglabel[TrainTestSampleIndex[[loop]]$test_index]
#########
#
# PerTestlabel<-PerTurboClassifiedlabel[[fold]][testlocation_index]
# PAcc<-bh_confusionmat(testlabel,PerTestlabel,option="CI")
# PerTurbo$OA<-rbind(PerTurbo$OA, PAcc$OA)
# PerTurbo$conf[[fold]]<-PAcc$conf
# PerTurbo$UA[[fold]]<-PAcc$UA
# PerTurbo$PA[[fold]]<-PAcc$PA
# # SVM
SVMTestlabel<-SVMClassifiedlabel[[fold]][testlocation_index]
SAcc<-bh_confusionmat(testlabel,SVMTestlabel,option="CI")
#kappa coefficient calcualtion
kapmat<-cbind(matrix(t(testlabel)),matrix(t(SVMTestlabel)))
kappacoef<-kappa2(kapmat)

SVM$OA<-rbind(SVM$OA, SAcc$OA) 
SVM$conf[[loop]]<-SAcc$conf
SVM$UA[[loop]]<-SAcc$UA
SVM$PA[[loop]]<-SAcc$PA
SVM$F1[[loop]]<-SAcc$F1
SVM$kappa[[loop]]<-kappacoef$value
# #
DTTestlabel<-DTClassifiedlabel[[fold]][testlocation_index]
DAcc<-bh_confusionmat(testlabel,DTTestlabel,option="CI")
DTAcc[fold]<-DAcc$OA[1]

DT$OA<-rbind(DT$OA, DAcc$OA) 
DT$conf[[fold]]<-DAcc$conf
DT$UA[[fold]]<-DAcc$UA
DT$PA[[fold]]<-DAcc$PA

# RF
RFTestlabel<-RFClassifiedlabel[[fold]][testlocation_index]
RAcc<-bh_confusionmat(testlabel,RFTestlabel,option="CI")
#kappa coefficient calcualtion
kapmat<-cbind(matrix(t(testlabel)),matrix(t(RFTestlabel)))
kappacoef<-kappa2(kapmat)

RFAcc[fold]<-RAcc$OA[1]

RF$OA<-rbind(RF$OA, RAcc$OA) 
RF$conf[[fold]]<-RAcc$conf
RF$UA[[fold]]<-RAcc$UA
RF$PA[[fold]]<-RAcc$PA

# NB
NBTestlabel<-NBClassifiedlabel[[fold]][testlocation_index]
NAcc<-bh_confusionmat(testlabel,NBTestlabel,option="CI")
NBAcc[fold]<-NAcc$OA[1]

NB$OA<-rbind(NB$OA, NAcc$OA) 
NB$conf[[fold]]<-NAcc$conf
NB$UA[[fold]]<-NAcc$UA
NB$PA[[fold]]<-NAcc$PA

# MLR
MLRTestlabel<-MLRClassifiedlabel[[fold]][testlocation_index]
MAcc<-bh_confusionmat(testlabel,MLRTestlabel,option="CI")
MLRAcc[fold]<-MAcc$OA[1]

MLR$OA<-rbind(MLR$OA, MAcc$OA) 
MLR$conf[[fold]]<-MAcc$conf
MLR$UA[[fold]]<-MAcc$UA
MLR$PA[[fold]]<-MAcc$PA

}

NPer<-500;
outer<-1
## Accuracy write in Excel
#setwd(old_dir)
pname<-"D:/PostDocWork/Denmark/ClassificationResults/ISPRS7_500samples/OrgImg+DSM+NDVI+NDVIAP/"

source('bh_confwriteexcel.R')
trp<-as.character(NPer[outer]);testp<-as.character(1-NPer[outer])
# # PeRTurbo
# option<-1
# fname=paste(pname, "ISPRS7_DSM_AP_PerTurbo_tr",trp,"_test",testp,".xlsx", sep="")
# bh_confwriteexcel(PerTurbo, fname,option)

# # SVM
# fname=paste(pname,"ISPRS7_OrgImg_NDVI_DSM_SVM_tr",trp,"_test",testp,".xlsx", sep="")
# option<-2
# setwd(old_dir)
# bh_confwriteexcel_cluster(SVM, fname,option)
# 
# 
# Decision Tree
fname=paste(pname,"ISPRS7_OrgImg+NDVI+AP_DT_tr",trp,"_test",testp,".xlsx", sep="")
#fname="ISPRS7_DT_10tr_90test.xlsx"
option<-3
bh_confwriteexcel(DT, fname,option)
# Random Forest
fname=paste(pname,"ISPRS7_OrgImg+NDVI+AP_RF_tr",trp,"_test",testp,".xlsx", sep="")
#fname="ISPRS7_RF_10tr_90test.xlsx"
option<-4
bh_confwriteexcel(RF, fname,option)
# Naive Bayes Forest
fname=paste(pname,"ISPRS7_OrgImg+NDVI+AP_NBC_tr",trp,"_test",testp,".xlsx", sep="")
#fname="ISPRS7_RF_10tr_90test.xlsx"
option<-5
bh_confwriteexcel(NB, fname,option)
# multinomial logistic reg classifier
fname=paste(pname,"ISPRS7_OrgImg+NDVI+AP_MLRC_tr",trp,"_test",testp,".xlsx", sep="")
#fname="ISPRS7_RF_10tr_90test.xlsx"
option<-6
bh_confwriteexcel(MLR, fname,option)

MeanAccuracy<-list();StdAccuracy<-list()
MeanAccuracy$DT<-colMeans(DT$OA)
StdAccuracy$DT<-sqrt(colVars(DT$OA))
MeanAccuracy$RF<-colMeans(RF$OA)
StdAccuracy$RF<-sqrt(colVars(RF$OA))
MeanAccuracy$NB<-colMeans(NB$OA)
StdAccuracy$NB<-sqrt(colVars(NB$OA))
MeanAccuracy$MLR<-colMeans(MLR$OA)
StdAccuracy$MLR<-sqrt(colVars(MLR$OA))
#
#
setwd(pname)
save(DT,file=paste("OrgImg_NDVI_DSM_NDVIAP_DT_Results_Percent_",as.character(NPer[outer]),sep=""))
save(RF,file=paste("OrgImg_NDVI_DSM_NDVIAP_RF_Results_Percent_",as.character(NPer[outer]),sep=""))
save(NB,file=paste("OrgImg_NDVI_DSM_NDVIAP_NBC_Results_Percent_",as.character(NPer[outer]),sep=""))
save(MLR,file=paste("OrgImg_NDVI_DSM_NDVIAP_MLR_Results_Percent_",as.character(NPer[outer]),sep=""))


setwd(pname)
TrainingPercent<-NPer;
save(MeanAccuracy, StdAccuracy,TrainingPercent,classifiers,
     file=paste("OrgImg_NDVI_DSMNDVIAP_ClassifiersClassificationAcc_", as.character(NPer[outer]), sep=""))
