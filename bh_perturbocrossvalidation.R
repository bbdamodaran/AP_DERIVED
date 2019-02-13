bh_perturbocrossvalidation<-function(label,Data,fold)
{
 
  
  
  C=c(0.9, 0.75, 0.5, 0.4, 0.2, 0.1, 0.01, 0.001, 0.0001, 0.00001, 0.000001)
  gamma<-c(seq(from=-10, to=0, by=1), seq(from=0.5, to=5, by=0.5),seq(from=6, to=10, by=1))
  MeshC<-rep(C, each=length(gamma))
  Meshgamma<-rep(gamma, times=length(C))
  
  
  
  # grid search and cross validation
  cv_error=matrix(0,length(MeshC),1)
  # for same random numbers for each new run
  seed<-0;
  set.seed(seed)
  
  cvpart<-generateCVRuns(label,ntimes=1, nfold=5, stratified = TRUE)
  POA<-numeric(0);cv_acc<-numeric(0)
  parameter<-list()
  for (i in 1:length(MeshC))
  {
    for (j in 1:fold)
    {
      tr_index<-cvpart[[1]][[j]]
      tr_label<-label[-tr_index];TrainData<-Data[-tr_index,]
      test_label<-label[tr_index];TestData<-Data[tr_index,]
      option="train"
      #parameter$sigma=1/(2*(2^Meshgamma[i])); # for Kernlab package
      parameter$sigma=2^Meshgamma[i]; # for bh_rbf function
      parameter$RegC=MeshC[i]
      source('bh_perturboclassification.R')
      W<-bh_perturboclassification(TrainData,tr_label,option,parameter)  
       
      # testing
      option="test"
      classifiedlabel<-bh_perturboclassification(TestData,W,option)
      source('bh_confusionmat.R')
      OA<-bh_confusionmat(test_label,classifiedlabel$classifiedlabel)
      POA[j]<-OA$OA
    }
    cv_acc[i]<-mean(POA)
  }
  
  # extraction of best parameters
  bestcvacc<-max(cv_acc)
  bestidx<-which.max(cv_acc)
  # bestidx=seq(1:length(MeshC))[cv_error==besterr] 
  
  perturboparam.RegC=MeshC[bestidx[1]]
  perturboparam.sigma=2^Meshgamma[bestidx[1]]
  perturboparam.bestcvacc=bestcvacc
  
  return(list(RegC=perturboparam.RegC,sigma=perturboparam.sigma,
              bestcvacc=perturboparam.bestcvacc ))
  
}# function end




