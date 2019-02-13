bh_statistics<-function(Data)
{
  mu=colMeans(Data);sigma=colSds(Data);
  
  return(list(mu=mu, sigma=sigma))
  
}