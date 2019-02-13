bh_rbf<-function(X1,sigma,X2)
{
  
  
  if (missing(X2))
  {
    m<-nrow(X1);n<-ncol(X1)
    M1<-X1%*%t(X1)
    D1<-as.matrix(diag(M1))
  
    K<-repmat(t(D1),m,1)+repmat(D1,1,m)-as.vector(2)*M1; 
    
    #K=repmat(diag(X1*X1')',m,1)+repmat(diag(X1*X1'),1,m)-2*(X1*X1');  
    
  }else 
    {
    m<-nrow(X1);n<-ncol(X1)
    p<-nrow(X2);q<-ncol(X2)
    D1<-as.matrix(diag(X1%*%t(X1)))
    D2<-as.matrix(diag(X2%*%t(X2)))
    
    K<-repmat((D2),m,1)+repmat(D1,1,p)-as.vector(2)*(X1%*%t(X2));
                                       
    #K=repmat(diag(X2*X2')',m,1)+repmat(diag(X1*X1'),1,p)-2*(X1*X2'); 
    
  }
  K<-exp((-1/(2*sigma))*K);
  return(K)
  
}# function end



