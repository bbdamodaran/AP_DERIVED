bh_perturboclassification<-function(Data,label,option,parameter)
{
  is.character(option)
  is.character("train")
  if (strcmp(option,"train"))    # training stage of perturbo algorithm
  {
  Nclass=length(unique(label));
  sigma=parameter$sigma;
  RegC=parameter$RegC;
 W<-list();
 for (i in 1:Nclass)
  {
  index=(label==i);
#   source('bh_rbf.R')
#   W$K[[i]]<-bh_rbf(Data[index,],sigma)
  #
   sigma<-1/(2*sigma)
   rbf<-rbfdot(sigma)
   W$K[[i]]<-kernelMatrix(rbf,Data[index,])
  }
 W$TrainData<-Data;
 W$train_label<-label;
 W$sigma<-sigma;
 W$RegC<-RegC;
 W$Nclass<-Nclass;
  
  return(W)
  
  }else if (strcmp(option,"test"))
  {
    #testing stage of perturbo method
    TestData<-Data; W<-label;
  rm(Data);rm(label)
  subsetpart=nrow(TestData)>5000; # if large data, the classification is performed by partitioning the Test Data
  Nclass=W$Nclass;
  nr=nrow(TestData);nc=ncol(TestData)
  if (is.list(TestData))
    {nr=nrow(TestData);nc=ncol(TestData)}
  DV<-matrix(0,nr,Nclass)
  
  for (j in 1:Nclass)
  {
  index=W$train_label==j;
  
  if (subsetpart==FALSE)
  {
    
      
    rbf<-rbfdot(sigma=1/(2*W$sigma))
   t1<-kernelMatrix(rbf,W$TrainData[index,],TestData)
#     source('bh_rbf.R')
#     t1<-bh_rbf(W$TrainData[index,],W$sigma,TestData)
  tnclass<-nrow(W$K[[j]])
  RK<-W$K[[j]]+eye(tnclass)*W$RegC
  ink<-chol2inv(RK)
  DV[,j]<-1-colSums(t1*(ink%*%t1))
   #DV(:,j)=1-diag(t1'*inK*t1);
} else
{
                  partition=5000;
                  Nsamples=nrow(TestData);
                  Npart=ceil(Nsamples/partition);
                  
                  for (k in 1:Npart)
                  {
                  st=((k-1)*partition)+1;
                  last=k*partition;
                  if (k==Npart)
                  {
                    last<-Nsamples
                  TData<-TestData[st:last,];
                  }
                  else {
                  TData<-TestData[st:last,];
                  }
                  #tic
#                   source('bh_rbf.R')
#                   t1<-bh_rbf(W$TrainData[index,],W$sigma,TData)
                  rbf<-rbfdot(sigma=1/(2*W$sigma))
                  t1<-kernelMatrix(rbf,W$TrainData[index,],TData)
                  
                  #toc
                  tnclass<-nrow(W$K[[j]])
                  RK<-W$K[[j]]+eye(tnclass)*W$RegC
                  ink<-chol2inv(RK)
                  DV[st:last,j]<-1-colSums(t1*(ink%*%t1))
                 
#                   %tic
#                   inK=invChol_mex(RK);
#                   DV(st:last,j)=1-sum(t1.*(inK*t1),1);
                  
  
  }
  }
  
  
  
  }
  label<-apply(DV,1,which.min)
  #[~,label]=min(DV,[],2);
  return(list(classifiedlabel=label,Probvalues=DV))
  

    
  
  }
  
}

