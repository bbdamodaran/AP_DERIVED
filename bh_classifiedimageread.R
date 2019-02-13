bh_classifiedimageread<-function()
{
  
  classifiedlabel<-list()
  
  fname<-"D:/PostDocWork/Denmark/lassificationResults/ISPRS7_500samples/OrgImg+DSM/SVM_PerTurbo/ISPRS_7_SVM_CLassifiedImage_500fold_"
  for (fold in 1:10)
  {
  fn<-paste(fname,as.character(fold),sep="")
  data<-read.ENVI(fn, headerfile = paste(fn,".hdr",sep=""))
  sz<-dim(data)
  classifiedlabel[[fold]]<-reshape(data,sz[1]*sz[2],sz[3])
  }
  return(classifiedlabel=classifiedlabel)
  
}