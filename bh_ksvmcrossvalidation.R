bh_ksvmcrossvalidation<-function(Data,label,kertype,fold)
{
  if (kertype==2)
{
  kerneltype="rbfdot"
}
   
  C<-seq(from=0,to=15,by=2)
if (kertype==2)
{
  gamma<-c(seq(from=-15, to=3, by=2))
  MeshC<-rep(C, each=length(gamma))
  Meshgamma<-rep(gamma, times=length(C))
}
  
  # grid search and cross validation
  kcv_error=matrix(0,length(MeshC),1)
  cv_error=matrix(0,length(MeshC),1)
  label<-as.factor(label)
  seed<-0;
  set.seed(seed)
  
  # parallel packages
  library(doParallel)
  library(parallel)
  Ncores<-detectCores()
  cl<-makeCluster(Ncores-2)
  registerDoParallel(cl)
  
 # for(i in 1:length(MeshC))
  cv_error<-foreach (i =1:length(MeshC), .combine=rbind, .packages = 'e1071')%dopar%
  {
    # Ksvm package
#     kmodel<-ksvm(Data,label,type="C-svc",kernel=kerneltype,C=2^MeshC[i], 
#                 kpar=list(sigma=2^Meshgamma[i]), cross=fold, prob.model=TRUE)
#     kcv_error[i]=cross(kmodel)
    #e1071 package
    require(e1071)
    tune.control(random = FALSE, nrepeat = 1, repeat.aggregate = mean,
                 sampling = "cross", sampling.aggregate = mean,
                 sampling.dispersion = sd,
                 cross = 5,performances = TRUE, error.fun = NULL)
    obj <- tune.svm(Data,label, gamma = 2^Meshgamma[i], cost = 2^MeshC[i],probability = TRUE )
    
    cv_error[i]=obj$best.performance
    obj$best.performance
  }
  stopCluster(cl)
  # extraction of best parameters
   besterr=min(cv_error)
   bestidx=seq(1:length(MeshC))[cv_error==besterr]                
   SVMparam.bestC=2^MeshC[bestidx[1]]
   SVMparam.bestsigma=2^Meshgamma[bestidx[1]]
   SVMparam.besterror=besterr

   return(list(SVMparam.bestC=SVMparam.bestC,SVMparam.bestsigma=SVMparam.bestsigma,
               SVMparam.besterror=SVMparam.besterror ))
  
  
}