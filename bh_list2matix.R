bh_list2matix<-function(ListData)
{
  Nlist<-length(ListData)
  classifiedlabel<-numeric(0);Probvalues<-numeric(0)
  for (i in 1:Nlist)
  {
    if (i==1)
    {classifiedlabel<-rbind(classifiedlabel,t(ListData[[i]]$classifiedlabel))}
    else
    {
    classifiedlabel<-cbind(classifiedlabel,t(ListData[[i]]$classifiedlabel))}
    Probvalues<-rbind(Probvalues,ListData[[i]]$Probvalues)
  }
  
  return(list(classifiedlabel=classifiedlabel,Probvalues=Probvalues))
}