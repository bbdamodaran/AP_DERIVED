bh_labelcoding<-function(train_label)
{
  nclass = length(unique(train_label))
  Nsamples = length(train_label)
  codelabel = -1*matrix(1, Nsamples, nclass)
  
  for (i in 1:nclass)
  {
    index = which(train_label==i)
    codelabel[index,i]=1
  }
  return(codelabel)
}