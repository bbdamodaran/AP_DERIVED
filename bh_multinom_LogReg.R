bh_multinom_LogReg<-function(Data,label,option)
  
  
  if (strcmp(option,"train"))
  {
    colnames(Data)<-paste0("feature", 1:ncol(Data))
    f<-as.formula(paste("label~",paste0("feature",1:ncol(Data), collapse = "+")))
    Data<-data.frame(Data)
    
    model<-multinom(f, data=Data)
    #rm(label)
    trainclassifiedlabel <- predict(model, newdata = Data, maxit=500)
    #trainclassifiedlabe<-apply(trainProb,1,which.max)
    return(list(model=model))
    
    
  }else if (strcmp(option,"test"))
  {
    TestData<-Data;model<-label
    rm(label)
    colnames(TestData)<-paste0("feature", 1:ncol(TestData))
    TestData<-data.frame(TestData)
    probvalues<- predict(model$model, newdata = TestData, "probs")
    
    classifiedlabel<-apply(probvalues,1,which.max)
    return(list(classifiedlabel=classifiedlabel,Probvalues=probvalues))
  }