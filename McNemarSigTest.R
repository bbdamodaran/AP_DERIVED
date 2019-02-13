
rm(list=ls())
source('bh_loadLib.R')
bh_loadLib()
library('irr')
# Load the original label
### ISPRS Dataset
# fname<-"D:/PostDocWork/Denmark/AttributeProfiles/ISPRS/ISPRS_7_GT_SampleLocation.mat"
# SampleLocationIndex<-readMat(fname)
# nam<-names(SampleLocationIndex)
# SampleLocationIndex<-SampleLocationIndex[[nam[1]]]
# fname<-"D:/PostDocWork/Denmark/AttributeProfiles/ISPRS/ISPRS_7_GT_Label_Classwise.mat"
# label<-readMat(fname)
# nam<-names(label)
# label<-label[[nam[1]]]
# ##
# sc<-sort(SampleLocationIndex, decreasing = FALSE, index.return=TRUE)
# nlabel<-label[sc$ix]
# #
# orglabel<-nlabel
# # Load the testing sample location Indexes
# fname<-"D:/PostDocWork/Denmark/AttributeProfiles/ClassificationResults/NDVI+NDVIAP_500/ISPRS_7_NDVIAP_TRTestSampleLocationIndex_500"
# sss<-load(fname)

############################## rCD Dataset
fname<-"D:/PostDocWork/Denmark/Datas/RCD/RCD30_TrainingClassImage.mat"
ClassImage<-readMat(fname)
nam<-names(ClassImage)
ClassImage<-ClassImage[[nam[1]]]
SampleLocationIndex<-which(ClassImage>0)
nlabel<-ClassImage[SampleLocationIndex]
orglabel<-nlabel
##


# Loading pre-generated testing and training location indexes
fname<-"D:/PostDocWork/Denmark/ClassificationResults/RCD_500samples/RCD30_OrgImg_TRTestSampleLocationIndex_500"
sss<-load(fname)

####################
# Load the classification labels of classifier 1
fname<-"D:/PostDocWork/Denmark/ClassificationResults/RCD_500samples/OrgImg+DSM+NDVI+NDVI_AP/RCD_SVM_OrgImg_NDVI_DSM_NDVIAP_Classifiedlabel_500"
# fname<-"D:/PostDocWork/Denmark/ClassificationResults/RCD_500samples/OrgImg+DSM+NDVI+NDVI_AP/RCD30_Classifiers_OrgImg_NDVI_NDSM_NDVIAP_Classifiedlabel500"
classifiedlabels<-load(fname)

P<-SVMClassifiedlabel

# Load the labels of classifier 2
fname2<-"D:/PostDocWork/Denmark/ClassificationResults/RCD_500samples/PC12_PC12AP/RCD_SVM_PC12PC12AP_Classifiedlabel_500"
load(fname2)





pvalue<-matrix(0, nrow=10, ncol=1)
chi2value<-matrix(0, nrow=10, ncol=1)

for (fold in 1:10)
{
  ######## ISPRS 7
#   testlocation_index=TrainTestSampleIndex[[fold]]$test_index;
#   testlabel<-orglabel[testlocation_index]
  #### RCD
    testlocation_index<-SampleLocationIndex[TrainTestSampleIndex[[fold]]$test_index];
  testlabel<-orglabel[TrainTestSampleIndex[[fold]]$test_index]
  #########
  
  PropSVMTestlabel<-P[[fold]][testlocation_index] 
  
  OrgAPSVMTestlabel<-SVMClassifiedlabel[[fold]][testlocation_index] 
  
  tmp_label1 = PropSVMTestlabel==testlabel
  tmp_label2 = OrgAPSVMTestlabel ==testlabel
  
  tmp_c = tmp_label1+tmp_label2
  A= sum(tmp_c==2)
  
  B = sum(tmp_label1==1)+sum(tmp_label2==0)
  C = sum(tmp_label1==0)+sum(tmp_label2==1)
  D = sum(tmp_label1==0)+sum(tmp_label2==0)
  
  CM =matrix(c(A,C,B,D), nrow=2, dimnames = list("Classifier1" =c("Correct", "Wrong"),
                                                 "Classifier2" = c("Correct", "Wrong")))
  MT = ((abs(C-B)-1)**2)/(B+C)
  
  sv<-mcnemar.test(CM, correct=FALSE)
  pvalue[fold]<-sv$p.value
  chi2value[fold]<-sv$statistic
  
  
}

MeanChi2Value = mean(chi2value)


