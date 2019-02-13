bh_confusionmat<-function(groundtruthlabel,classifiedlabel,option)
  # Calculates the confusion matrix
{
  if (missing(option))
    option="WithoutCI"
  ## ++++++++++++++++++++++++++++ class names ++++++++++++++++
  Nclass<-length(unique(groundtruthlabel))
  classnames<-list("Imp_surf","Building","Low_Veg","Tree","Car") # ISPRS_7
  #classnames<-list("Building", "Hed&Bush","Grass","Road&Parklot","Tree","Wall&Carport")
  classnames<-paste("class",1:Nclass,sep="")
 
 
  
  conf<-matrix(0,Nclass, Nclass)
  
  for (i in 1:Nclass)
  {
  dummy=classifiedlabel[groundtruthlabel==i]
  for (j in 1:Nclass)
  { 
    conf[i,j]=sum(dummy==j)
  }
  }
  Ncorrect<-sum(diag(conf))
  Nsamples<-sum(rowSums(conf))
  OA<-Ncorrect/ Nsamples  # overall accuarcy
  PA<-diag(conf)/rowSums(conf)              # producer accuarcy
  UA<-diag(conf)/colSums(conf)             # user accuarcy
  # Naming
  colnames(conf)<-classnames;rownames(conf)<-classnames
  #names(PA)<-classnames;names(UA)<-classnames
  F1<-(2*diag(conf))/(rowSums(conf)+colSums(conf))
  
  if (option=="CI")
  {
    library(binom)
    COA<-binom.confint(Ncorrect, Nsamples, conf.level = 0.95,method="lrt")
    COA<-cbind(OA, COA$lower, COA$upper)
    CPA<-binom.confint(diag(conf),rowSums(conf),conf.level = 0.95,method="lrt")
    CPA<-cbind(PA, CPA$lower, CPA$upper)
    CUA<-binom.confint(diag(conf),colSums(conf),conf.level = 0.95,method="lrt")
    CUA<-cbind(UA, CUA$lower, CUA$upper)
    CF1<-binom.confint(2*diag(conf), (rowSums(conf)+colSums(conf)), conf.level = 0.95,method="lrt")
    CF1<-cbind(F1, CF1$lower, CF1$upper)
    nam<-list("Accuray","LBound","UBound")
   colnames(COA)<-nam;colnames(CPA)<-nam;colnames(CUA)<-nam;
   colnames(conf)<-classnames;rownames(conf)<-classnames
   rownames(CPA)<-classnames;rownames(CUA)<-classnames
    return(list(OA=COA, conf=conf,PA=CPA, UA=CUA,F1=CF1))
    } else
    {
     return(list(OA=OA, conf=conf,PA=PA,UA=UA,F1=F1))
    }
}