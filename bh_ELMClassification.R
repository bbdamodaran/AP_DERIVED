bh_ELMClassification<-function(A,tr_label,option,nhid)
{
  
  if (strcmp(option,"train")) 
  {
    source('bh_labelcoding.R')
    codetrain_label <- bh_labelcoding(tr_label)
    nclass = ncol(codetrain_label)
    
    actfun = "radbas"
    TRAIN =1
    inode = ncol(A)
    source('bh_ELMCLASS.R')
    W=bh_ELMCLASS(A,codetrain_label,TRAIN,inode,actfun,nhid,nclass)
    return(W)
    
  }else if (strcmp(option,"test"))
  {
    TRAIN =0
    source('bh_ELMCLASS.R')
    W = bh_ELMCLASS(A, tr_label, TRAIN)
    return(W)
  }
  
  
  
}