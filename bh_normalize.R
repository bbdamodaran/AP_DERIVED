bh_normalize<-function(Data, params)
{
  ##
  if (missing(params))
  {
    # for training data
  mu=colMeans(Data);sigma=colSds(Data);
  }else
  {
    mu=params$mu; sigma=params$sigma
  }
  sz<-dim(Data)
  NData<-matrix(0,nrow(Data),ncol(Data))
  #NData<-(Data-repmat(mu, nrow(Data),1))/(repmat(sigma,nrow(Data),1))
  
  if (sz[1]<=5000)
  {
    NData<-(Data-repmat(mu, nrow(Data),1))/(repmat(sigma,nrow(Data),1))
  } else
  {
    partition=5000;
    Nsamples=nrow(Data);
    Npart=ceil(Nsamples/partition);
    
    for (k in 1:Npart)
    {
      st=((k-1)*partition)+1;
      last=k*partition;
      if (k==Npart)
      {
        last<-Nsamples
         
      }
     
      NData[st:last,]<-(Data[st:last,]-repmat(mu, nrow(Data[st:last,]),1))/(repmat(sigma,nrow(Data[st:last,]),1))
      
    
    
  }
  
  } 
  
  ## [0-1] scaling
  #NData<-Data-repmat(col.min(Data), nrow(Data),1)/repmat()
  return(NData)
}

