bh_svmclassification<-function(Data,label,option,kertype,parameter)
{
 
  #
  if (strcmp(option,"train"))
  {
    if (kertype==0)
      kerneltype="Linear"
    else if (kertype==1)
      kerneltype="poly"
    else if (kertype==2)
      kerneltype="rbfdot"
#     
#     model<-ksvm(Data,label,type="C-svc",kernel=kerneltype,C=parameter$SVMparam.bestC,
#                 kpar=list(sigma=parameter$SVMparam.bestsigma), prob.model=TRUE)
    
    # e107 package
    model <-   svm(Data,label, type="C-classification", kernel= 
                   "radial", gamma=parameter$SVMparam.bestsigma,cost=parameter$SVMparam.bestC, probability = TRUE )
    
    return(list(model=model))
    #return(list(model=model, error=error(model))) # for ksvm
  }else if (strcmp(option,"test"))
{
    model<-Data;TestData<-label;
    rm(label)
   #classifiedlabel<-predict(model$model,TestData) 
#       Probvalues<-predict(model$model,TestData,type="probabilities", coupler="pkpd" ) 
#       classifiedlabel<-max.col(Probvalues)
      
      # e107 package
      ## svm prediction
      require(e1071)
     # classifiedlabel <- predict(model$model,TestData)
      Probvalues <- predict(model$model,TestData,probability = TRUE)
      ## find the maximum of predicted probability 
      classifiedlabel <- max.col(attributes(Probvalues)$probabilities)
   return(list(classifiedlabel=classifiedlabel, Probvalues=attributes(Probvalues)$probabilities))
    
  }
  
  
  
  
}