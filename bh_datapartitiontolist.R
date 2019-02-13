bh_datapartitiontolist<-function(Data,nparition)
{
  # This function paritions the data into the list
  
  
  partition=nparition;
  Nsamples=nrow(Data);
  Npart=ceil(Nsamples/partition);
  
  library(parallel)
  library(doParallel)
  Ncores<-detectCores()
  cl<-makeCluster(Ncores-1)
  registerDoParallel(cl)
  
  
  ListTestData<-foreach (k =1:Npart)%dopar%
  {
    st=((k-1)*partition)+1;
    last=k*partition;
    if (k==Npart)
    {
      last<-Nsamples
    }
     
    Data[st:last,];
    
  }
  stopCluster(cl)
  
  return(ListTestData)
}