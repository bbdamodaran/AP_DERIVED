rm(list=ls())
old_dir<-setwd("/share/home/damodara/DenmarkWork/RCodes")
getwd()
source('bh_loadLib.R')
bh_loadLib()
#### ++++++++++ Options+++++++++++++++
FullImageClassification=TRUE
TestingSampClassification=FALSE

### ++++++++++++++ Read the Data or Image +++++++++++++++++++++++++

## Read the training samples file (CSV Files)
#fname<-"/share/home/damodara/DenmarkWork/Data/Sebastien/train_all_6.csv"
#data<-read.csv(fname)
#featurenames=names(data)
#newdata<-data.matrix(data)
#label=newdata[,length(featurenames)]
#ndata<-newdata[,c(4,5)]

## Read the data from mat format
# fname<-"/share/home/damodara/DenmarkWork/Data/ISPRS_7/AP/ISPRS7_NDVI_AP_All.mat"
# data<-readMat(fname)
# nam<-names(data)
# data<-data[[nam[1]]]
# sz<-dim(data)
# ndata<-reshape(data,sz[1]*sz[2],sz[3])
# rm(data)
## Read the data from the envi file
fname<-"/share/home/damodara/DenmarkWork/Data/ISPRS_7/AP/NDVIALL_demo"
data<-read.ENVI(fname, headerfile = paste(fname,".hdr",sep=""))
sz<-dim(data)
sz
ndata<-reshape(data,sz[1]*sz[2],sz[3])
rm(data)
## Read the NDVI File in as image
fname<-"/share/home/damodara/DenmarkWork/Data/ISPRS_7/ISPRS7_NDVI.mat"
NDVIData<-readMat(fname);nam<-names(NDVIData)
NDVIData<-NDVIData[[nam[1]]]
NDVIData<-reshape(NDVIData,sz[1]*sz[2])
# Append NDVI data to the AP of NDVI data
ndata<-cbind(NDVIData,ndata)
rm(NDVIData)
####
fname<-"/share/home/damodara/DenmarkWork/Data/ISPRS_7/ISPRS_7_GT_SampleLocation.mat"
SampleLocationIndex<-readMat(fname)
nam<-names(SampleLocationIndex)
SampleLocationIndex<-SampleLocationIndex[[nam[1]]]
fname<-"/share/home/damodara/DenmarkWork/Data/ISPRS_7/ISPRS_7_GT_Label_Classwise.mat"
label<-readMat(fname)
nam<-names(label)
label<-label[[nam[1]]]
##
sc<-sort(SampleLocationIndex, decreasing = FALSE, index.return=TRUE)
nlabel<-label[sc$ix]
#
nr=nrow(ndata);nc=ncol(ndata)
orglabel<-nlabel

source('bh_normalize.R')
ndata<-bh_normalize(ndata)
ndata[is.na(ndata)]<-0
dim(ndata)
# Random subset the samples
# nsamples=length(label);
# randperum=sample(nsamples);
# ntrain=round(nsamples*0.8)
# subsetindex=randperum[1:ntrain];
# newdata<-ndata[randperum,]

source('bh_randomsubsetsamples.R')

NPer= 2000 #seq(0.2,0.9, by=0.1)
MeanAccuracy<-list();StdAccuracy<-list()

for (outer in 1:length(NPer))
{
  Acc<-numeric(0)
  PerTurboAcc<-numeric(0)
  DTAcc<-numeric(0)
  RFAcc<-numeric(0)
  #
  PerTurbo<-list();PerTurboDV<-list();
  PerTurboClassifiedlabel<-list();PerTurboTrainingParameter<-list();
  PerTurbo$OA<-numeric(0);
  for (loop in 1)#:10)  # loop for number of folds
  {
    # sss<-bh_randomsubsetsamples(orglabel,"Percentage",NPer[outer])
    sss<-bh_randomsubsetsamples(orglabel,"No_of_samples",2000)
    testlocation_index=sss$test_index;
    TrainData<-ndata[sss$tr_index,];train_label<-orglabel[sss$tr_index]
    s<- dim(TrainData)
    cat("\n dim of TrainData.",s, "\n\n")
    testlabel<-orglabel[sss$test_index]
    if (FullImageClassification==TRUE)
    {
      TestData<-ndata
    }else{
      TestData<-ndata[-sss$tr_index,];
    }
    
    # Testing Data Parition
    source('bh_datapartitiontolist.R')
    Lndata<-bh_datapartitiontolist(TestData,20000)
    rm(TestData)
    
    # Decision Tree
    source('bh_decisiontreeclassification.R')
    option="train"
    DTmodel<-bh_decisiontreeclassification(TrainData,train_label,option)
    source('bh_loadparallel_Lib.R')
    cl<-bh_loadparallel_Lib()
    DTPer<-foreach(testpar=1:length(Lndata),.packages = c('rpart','pracma')) %dopar%
    {
      TData<-Lndata[[testpar]]
      DTPrediction<-bh_decisiontreeclassification(DTmodel,TData,option="test")
      return(DTPrediction)
    }
    stopCluster(cl)
    #
    DTPrediction<-bh_list2matix(DTPer)
    rm(DTPer)
    # Classification Results
    DTDV[[loop]]<-DTPrediction$Probvalues
    DTClassifiedlabel[[loop]]<-DTPrediction$classifiedlabel
    DTTrainingParameter[[loop]]<-DTmodel
    # confusion matrix
    DAcc<-bh_confusionmat(testlabel,DTPrediction$classifiedlabel[sss$test_index],option="CI")
    
    
    DTAcc[loop]<-DAcc$OA[1]
    
    DT$OA<-rbind(DT$OA, DAcc$OA) 
    DT$conf[[loop]]<-DAcc$conf
    DT$UA[[loop]]<-DAcc$UA
    DT$PA[[loop]]<-DAcc$PA
    
    ## Random Forest
    source('bh_randomforest.R')
    option="train"
    str<-Sys.time()
    RFmodel<-bh_randomforest(TrainData,train_label,option)
    print(Sys.time()-str)
    
    #
    source('bh_loadparallel_Lib.R')
    cl<-bh_loadparallel_Lib()
    RFPer<-foreach(testpar=1:length(Lndata),.packages = c('rpart','pracma')) %dopar%
    {
      TData<-Lndata[[testpar]]
      RFPrediction<-bh_randomforest(RFmodel,TData,option="test")
      return(RFPrediction)
    }
    stopCluster(cl)
    #RFPrediction<-bh_randomforest(RFmodel,TestData,option="test")
    RFPrediction<-bh_list2matix(RFPer)
    rm(RFPer)
    # Classification results
    RFDV[[loop]]<-RFPrediction$Probvalues
    RFClassifiedlabel[[loop]]<-RFPrediction$classifiedlabel
    RFTrainingParameter[[loop]]<-RFmodel
    #confusion matrix
    RAcc<-bh_confusionmat(testlabel,RFPrediction$classifiedlabel[sss$test_index],option="CI")
    RFAcc[loop]<-RAcc$OA[1]
    
    RF$OA<-rbind(RF$OA, RAcc$OA) 
    RF$conf[[loop]]<-RAcc$conf
    RF$UA[[loop]]<-RAcc$UA
    RF$PA[[loop]]<-RAcc$PA
    
    ## Naive bayes classifier
    source('bh_naivebayes.R')
    NBmodel<-bh_naivebayes(TrainData,train_label,option='train')
    # testing
    option<-'test'
    source('bh_loadparallel_Lib.R')
    cl<-bh_loadparallel_Lib()
    NBPer<-foreach(testpar=1:length(Lndata),.packages = c('e1071','pracma')) %dopar%
    {
      TData<-Lndata[[testpar]]
      NBPrediction<-bh_naivebayes(TData,NBmodel,option="test")
      return(NBPrediction)
    }
    stopCluster(cl)
    
    NBPrediction<-bh_list2matix(NBPer)
    rm(NBPer)
    NBDV[[loop]]<-NBPrediction$Probvalues
    NBClassifiedlabel[[loop]]<-NBPrediction$classifiedlabel
    NBTrainingParameter[[loop]]<-NBmodel
    #confusion matrix
    NAcc<-bh_confusionmat(testlabel,NBPrediction$classifiedlabel[sss$test_index],option="CI")
    NBAcc[loop]<-NAcc$OA[1]
    
    NB$OA<-rbind(NB$OA, NAcc$OA) 
    NB$conf[[loop]]<-NAcc$conf
    NB$UA[[loop]]<-NAcc$UA
    NB$PA[[loop]]<-NAcc$PA
    
    ## Multinomial logistic regression classifier
    source('bh_multinom_LogReg.R')
    # training
    option<-'train'
    MLRmodel<-bh_multinom_LogReg(TrainData,train_label,option)
    # testing
    source('bh_loadparallel_Lib.R')
    cl<-bh_loadparallel_Lib()
    MLRPer<-foreach(testpar=1:length(Lndata),.packages = c('nnet','pracma')) %dopar%
    {
      TData<-Lndata[[testpar]]
      MLRPrediction<-bh_multinom_LogReg(TData,MLRmodel,option="test")
      return(MLRPrediction)
    }
    stopCluster(cl)
    
    MLRPrediction<-bh_list2matix(NBPer)
    rm(MLRPer)
    MLRDV[[loop]]<-MLRPrediction$Probvalues
    MLRClassifiedlabel[[loop]]<-MLRPrediction$classifiedlabel
    MLRTrainingParameter[[loop]]<-MLRmodel
    #confusion matrix
    MAcc<-bh_confusionmat(testlabel,MLRPrediction$classifiedlabel[sss$test_index],option="CI")
    MLRAcc[loop]<-MAcc$OA[1]
    
    MLR$OA<-rbind(MLR$OA, MAcc$OA) 
    MLR$conf[[loop]]<-MAcc$conf
    MLR$UA[[loop]]<-MAcc$UA
    MLR$PA[[loop]]<-MAcc$PA
    
    
  }
  ## Accuracy write in Excel
  
  source('bh_confwriteexcel_cluster.R')
  # PerTurbo
  # Decision Tree
  fname=paste("ISPRS7_NDVI_AP_DT_tr",trp,"_test",testp,".xlsx", sep="")
  #fname="ISPRS7_DT_10tr_90test.xlsx"
  option<-3
  setwd(old_dir)
  bh_confwriteexcel_cluster(DT, fname,option)
  # Random Forest
  fname=paste("ISPRS7_NDVI_AP_RF_tr",trp,"_test",testp,".xlsx", sep="")
  #fname="ISPRS7_RF_10tr_90test.xlsx"
  option<-4
  setwd(old_dir)
  bh_confwriteexcel_cluster(RF, fname,option)
  # Naive Bayes Forest
  fname=paste("ISPRS7_NDVI_AP_NBC_tr",trp,"_test",testp,".xlsx", sep="")
  #fname="ISPRS7_RF_10tr_90test.xlsx"
  option<-5
  setwd(old_dir)
  bh_confwriteexcel_cluster(NB, fname,option)
  # multinomial logistic reg classifier
  fname=paste("ISPRS7_NDVI_AP_MLRC_tr",trp,"_test",testp,".xlsx", sep="")
  #fname="ISPRS7_RF_10tr_90test.xlsx"
  option<-6
  setwd(old_dir)
  bh_confwriteexcel_cluster(MLR, fname,option)
  ########### ++++++++++++++++++++++++++++++++++++++++++++#################
  MeanAccuracy$DT[[outer]]<-colMeans(DT$OA)
  StdAccuracy$DT[[outer]]<-sqrt(colVars(DT$OA))
  MeanAccuracy$RF[[outer]]<-colMeans(RF$OA)
  StdAccuracy$RF[[outer]]<-sqrt(colVars(RF$OA))
  MeanAccuracy$NB[[outer]]<-colMeans(NB$OA)
  StdAccuracy$NB[[outer]]<-sqrt(colVars(NB$OA))
  MeanAccuracy$MLR[[outer]]<-colMeans(MLR$OA)
  StdAccuracy$MLR[[outer]]<-sqrt(colVars(MLR$OA))
  #
  setwd("/share/home/damodara/DenmarkWork/RCodes/Results/ISPRS_7/AP/NDVI_AP")
  save(DT,file=paste("NDVI_AP_DT_Results_Percent_",as.character(NPer[outer]),sep=""))
  save(RF,file=paste("NDVI_AP_RF_Results_Percent_",as.character(NPer[outer]),sep=""))
  save(NB,file=paste("NDVI_AP_NBC_Results_Percent_",as.character(NPer[outer]),sep=""))
  save(MLR,file=paste("NDVI_AP_MLR_Results_Percent_",as.character(NPer[outer]),sep=""))
  ######## ++++++ Decison values and labels write
  # Decision values
  classifiers<-c("DecisionTree","RandomForest","NaiveBayes","MultiNomReg")
  save(DTDV, RFDV, NBDV,MLRDV, classifiers, file=paste("ISPRS_Classifiers_DV_NDVIAP_",
                                                       as.character(NPer[outer]), sep=""))
  # Classifiedlabels
  save(DTClassifiedlabel, RFClassifiedlabel, NBClassifiedlabel, MLRClassifiedlabel,classifiers,
       file=paste("ISPRS_Classifiers_NDVI_AP_Classifiedlabel_", as.character(NPer[outer]), sep=""))
  setwd(old_dir)
  
}

setwd("/share/home/damodara/DenmarkWork/RCodes/Results/ISPRS_7/AP/NDVI_AP")
TrainingPercent<-NPer;
save(MeanAccuracy, StdAccuracy,TrainingPercent,classifiers,
     file=paste("NDVI_AP_ClassifiersClassificationAcc_", as.character(NPer[outer]), sep=""))