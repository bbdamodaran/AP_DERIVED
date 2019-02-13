bh_randomforest <-function(Data, label,option)
  ## Option -->"train" performs training of the random forest
  # Data --> Input training data
  # label --> training labels
  ## option--> "test" performs testing of the random forest 
  # Data --> trained function (fitted function) of the random forest
  # label --> Testing Data
  
{
  library(randomForest)
  
  if (strcmp(option,"train"))
  {
    # Pre-processing data format and formula for RF classifier
    label<-as.factor(label)
    colnames(Data)<-paste0("feature", 1:ncol(Data))
    f<-as.formula(paste("label~",paste0("feature",1:ncol(Data), collapse = "+")))
    Data<-data.frame(Data)
    
    ### training stage of the RF classifier##########
    # fine tunning number of variables to split
#     mtry <- tuneRF(Data,label, ntreeTry=200,
#                    stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
    # training random forest classifier
    set.seed(0) 
    # serial
#     Rf <-randomForest(f, data=Data, ntree=500, keep.forest=TRUE, importance=TRUE) 
#     print(Rf)
#     # parallel packages
    library(doParallel)
    library(parallel)
    Ncores<-detectCores()
    cl<-makeCluster(Ncores-1)
    registerDoParallel(cl)
#     # parallel
    Rf<-foreach(ntree=rep(ceil(500/(Ncores-1)),Ncores-1), .combine=combine, .packages = 'randomForest')%dopar%
    {
      randomForest(f, data=Data, ntree=ntree, keep.forest=TRUE, importance=TRUE)
    }
    stopCluster(cl)
    print(Rf)
    #classification of training samples
    trainclassifiedlabel <- predict(Rf, newdata = Data)
    
    return(list(model=Rf))
    
    
  }else if (strcmp(option,"test"))
  {
    # chaning format of the data's for RF prediction
    Rf<-Data;TestData<-label
    rm(label)
    colnames(TestData)<-paste0("feature", 1:ncol(TestData))
    TestData<-data.frame(TestData)
    # prediction of RF classifier
    probvalues<- predict(Rf$model, newdata = TestData, type="prob")
    classifiedlabel<-max.col(probvalues)
    
    return(list(classifiedlabel=classifiedlabel,Probvalues=probvalues))
  }
  
  
  
  
  
}