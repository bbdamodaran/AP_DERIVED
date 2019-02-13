bh_decisiontreeclassification<-function(Data,label,option)
{
  library(rpart)
  
  if (strcmp(option,"train"))
  {
    colnames(Data)<-paste0("feature", 1:ncol(Data))
    f<-as.formula(paste("label~",paste0("feature",1:ncol(Data), collapse = "+")))
    Data<-data.frame(Data)
        
    fit<-rpart(f, data=Data,method = "class")
    opt <- which.min(fit$cptable[,"xerror"])
    cp <- fit$cptable[opt, "CP"]
    fit_prune <- prune(fit, cp = cp)
    #fit_prune<-Data;TestData<-label
    #rm(label)
    trainclassifiedlabel <- predict(fit_prune, newdata = Data)
    return(list(model=fit_prune))
    
    
  }else if (strcmp(option,"test"))
  {
    fit_prune<-Data;TestData<-label
    rm(label)
    colnames(TestData)<-paste0("feature", 1:ncol(TestData))
    TestData<-data.frame(TestData)
    probvalues<- predict(fit_prune$model, newdata = TestData)
    classifiedlabel<-max.col(probvalues)
    
    
    return(list(classifiedlabel=classifiedlabel,Probvalues=probvalues))
  }
  
  
  
  
  
}
