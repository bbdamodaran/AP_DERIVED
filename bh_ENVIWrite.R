bh_ENVIWrite<-function(Image, filename)
{
  write.ENVI(Image,filename, interleave = c("bsq"))
  
}