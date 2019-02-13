bh_readsamples<-function(Image, labelImage)
{
  library(pracma)
  library(R.matlab)
  
  if (nargs()==0)
  {
    setwd('D:/Datasets/HSI')
    fname<-choose.files(caption = "Choose the Input Image")
    Image<-readMat(fname)
    nam<-names(Image)
    Image<-Image[[nam[1]]]
    
    #
    fname<-choose.files(caption = "Choose the class label image")
    labelImage<-readMat(fname)
    nam<-names(labelImage)
    labelImage<-labelImage[[nam[1]]]
  }
  
    SampleLocationIndex<-which(labelImage>0)
    train_label<-labelImage[SampleLocationIndex]
    
    if (length(dim(Image))>2)
    {
      sz<-dim(Image)
      Image = reshape(Image,sz[1]*sz[2], sz[3])
    }
    TrainData = Image[SampleLocationIndex,]
  
    return (list(TrainData=TrainData, train_label=train_label, SampleLocationIndex=SampleLocationIndex))
  }
  
  
  
}

