bh_ELMCLASS<-function(A,train_label,TRAIN,inode,actfun,nhid,class)
{
# %% Function Trains ans Test the ELM classifier
# % if TRAIN ==1 , it performs training
# % A -- Input datafile containing training samples and target values
# % innode -- number of features in the input data
# % type -- Type of Activation function (Unipolar, sigmoid, RBF)
# % class -- number of classes
# 
# % Output
# % W --- Training parameters in structure format
# 
# % if TRAIN ==0
# % A --- Trained parameters in the structure format
# % innode -- number of features in the input data
# % type -- Type of Activation function (Unipolar, sigmoid, RBF)
#   % lamda -- default value 0.001
# % class -- number of classes
# % TestData -- Data samples for testing (It doesn't contain the ground
#                                         % truth labels as the last column)
# 

if (TRAIN==1)
{
  
  T <- t(train_label)
  P <- t(A)
  #inpweight<-matrix(runif(nhid*inode,min=-1,max=1), ncol=inode)
  seed<-0;
  set.seed(seed)
  inpweight <- 2.0*rand(nhid,inode)-1.0
  
  tempH <- inpweight %*% P
  #biashid <- runif(nhid,min=0,max=1)
  seed<-0;
  set.seed(seed)
  biashid <-rand(nhid,1)
  biasMatrix <- matrix(rep(biashid, ncol(P)), nrow=nhid, ncol=ncol(P), byrow = F) 
  
  tempH = tempH + biasMatrix
  
  if(actfun == "sig") H = 1 / (1 + exp(-1*tempH))
  else {
    if(actfun == "sin") H = sin(tempH)
    else {
      if(actfun == "radbas") H = exp(-1*(tempH^2))
      else {
        if(actfun == "hardlim") H = hardlim(tempH)
        else {
          if(actfun == "hardlims") H = hardlims(tempH)
          else {
            if(actfun == "satlins") H = satlins(tempH)
            else {
              if(actfun == "tansig") H = 2/(1+exp(-2*tempH))-1
              else {
                if(actfun == "tribas") H = tribas(tempH)
                else {
                  if(actfun == "poslin") H = poslin(tempH)
                  else {
                    if(actfun == "purelin") H = tempH
                    else stop(paste("ERROR: ",actfun," is not a valid activation function.",sep=""))
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
#  outweight <- ginv(t(H), tol = sqrt(.Machine$double.eps)) %*% t(T)
#   Y <- t(t(H) %*% outweight)
   outweight <-pinv(t(H)) %*% t(T)
  
  
  model = list(inpweight=inpweight,biashid=biashid,outweight=outweight,
               actfun=actfun,nhid=nhid)
  return(model)
  
  
  
}else if (TRAIN==0)
{
  TestData<-A; model<-train_label
  rm(A, train_label)
  
  inpweight <- model$inpweight
  biashid <- model$biashid
  outweight <- model$outweight
  actfun <- model$actfun
  nhid <- model$nhid
  
  TV.P <- t(TestData)
  
  tmpHTest=inpweight %*% TV.P
  
  biasMatrixTE <- matrix(rep(biashid, ncol(TV.P)), nrow=nhid, ncol=ncol(TV.P), byrow = F)
  
  tmpHTest = tmpHTest + biasMatrixTE
  
  if(actfun == "sig") HTest = 1 / (1 + exp(-1*tmpHTest))
  else {
    if(actfun == "sin") HTest = sin(tmpHTest)
    else {
      if(actfun == "radbas") HTest = exp(-1*(tmpHTest^2))
      else {
        if(actfun == "hardlim") HTest = hardlim(tmpHTest)
        else {
          if(actfun == "hardlims") HTest = hardlims(tmpHTest)
          else {
            if(actfun == "satlins") HTest = satlins(tmpHTest)
            else {
              if(actfun == "tansig") HTest = 2/(1+exp(-2*tmpHTest))-1
              else {
                if(actfun == "tribas") HTest = tribas(tmpHTest)
                else {
                  if(actfun == "poslin") HTest = poslin(tmpHTest)
                  else {
                    if(actfun == "purelin") HTest = tmpHTest
                    else stop(paste("ERROR: ",actfun," is not a valid activation function.",sep=""))
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  DV = t(HTest) %*% outweight
  label = apply(DV, 1, which.max)
  
  return(list(classifiedlabel=label,Probvalues= DV))
  
  
  
  
  
   
}


}