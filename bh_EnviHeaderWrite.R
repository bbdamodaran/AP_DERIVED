bh_EnviHeaderWrite<-function(Image,hdrfile,description)
{
  if (missing(description))
  {
    description<-hdrfile
  }
  
  hdrfile<-paste(hdrfile,".hdr",sep="")
  
  def<-list()
  def$description<-description
  def$samples<-ncol(Image)
  def$lines<-nrow(Image)
  def$bands<-1
  def$header_offset<-0
  def$file_type = "ENVI Classification"
  def$data_type = 5
  def$interleave = "bsq"
  def$sensor_type = "Unknown"
  def$byte_order = 0
  def$x_start = 365
  def$y_start = 189

  # Pavia University
#   def$classes = 10
#   def$class_lookup = '{0,0,0, 238,210,238, 0,255,0, 0,255,255, 0,139,0, 255,0,255, 160,82,45, 160,32,240, 255,0,0, 255,255,0}'
#   def$class_names = '{Unclassified, Asphalt,  Meadows, Gravel, Trees, Metal Sheets, Bare soil, 
# Bitumen, Self building blocks, Shadow}'
 
  # ISPRS 7
    def$classes=5
  def$class_lookup<- '{0,0,0, 255,255,255, 0,0,255, 0,255,255, 0,255,0, 255,255,0}'
  def$class_names<-'{Unclassified,Impervious surfaces,Building,Low vegetation, Tree, Car}'
  
  # ISPRS 7
#   def$classes=6
#   def$class_lookup<- '{0,0,0, 255,255,255, 0,0,255, 0,255,255, 0,255,0, 255,255,0, 255,0,0}'
#   def$class_names<-'{Unclassified,Impervious surfaces,Building,Low vegetation, Tree, Car, Clutter/background}'
  
  params<-names(def);
  
  fid<-file(hdrfile,'w');
  writeLines('ENVI',fid,sep="\n")
  #fprintf(fid,'%s\n','ENVI');
  
  for (idx in 1:length(params))
  {
  param = params[idx];
  value<-def[[param]]
  #value = getfield(info,param);
  ind<-findstr(param,'_')
  if (!is.null(ind))
  {
  substring(param,ind,ind)<-c(" ")
  }
  #param(findstr(param,'_')) = ''; #automatic name
  line<-paste(param, '=',as.character(value))
  #line=[param,' = ',num2str(value)];
  writeLines(line,fid, sep="\n")
  #fprintf(fid,'%s\n',line);
  
  }
  
  close(fid);
  
  
  
}