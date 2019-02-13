bh_loadparallel_Lib<-function()
{
  # Loading parallel libraries
  library(parallel)
  library(doParallel)
  Ncores<-detectCores()
  cl<-makeCluster(Ncores-1)
  registerDoParallel(cl)
  return(cl)
}