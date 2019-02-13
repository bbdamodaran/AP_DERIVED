bh_randomsubsetsamples <-function(label,options,N)
  {
  m<-length(label);
  tr_index<-numeric(0);
  Nsamples<-numeric(0);
  Nclass<-length(unique(label));
  if (strcmp(options,"No_of_samples"))
{
  Nclass<-length(unique(label));
  for (i in 1:Nclass )
  {
    dummy<-seq(along=label)[label==i]
    Nsamples[i]<-length(dummy)
    Rindex<-sample(Nsamples[i]);
    index<-dummy[Rindex[1:N]];
    tr_index<-append(tr_index,index)
  }
  test_index<-seq(1:m)[-tr_index]
  }
  else if (strcmp(options,"Percentage"))
{
  for (i in 1:Nclass)
  {
  dummy<-seq(along=label)[label==i]
  Nsamples[i]<-length(dummy)
  Rindex<-sample(Nsamples[i]);
  Nextract<-round(Nsamples[i]*N);
  index<-dummy[Rindex[1:Nextract]];
  tr_index<-append(tr_index,index)
  }
  test_index<-seq(1:m)[-tr_index]
  
} 
  return(list(tr_index=tr_index,test_index=test_index))
}