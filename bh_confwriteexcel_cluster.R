bh_confwriteexcel_cluster<-function(AccMatrix, fname,option)
{
library(xlsx)
  
classifier<-list("PerTurbo","SVM","DecTree","RandForst")

nfolds<-nrow(AccMatrix$OA)
sheetnames<-paste0(classifier[option],"Fold",seq(from=1,to=nfolds, by=1))

# create workbooks and styles
setwd("/share/home/damodara/DenmarkWork/RCodes/Results/ISPRS_7")

wp<-createWorkbook(type="xlsx")
# col and row styles
cs1 <- CellStyle(wp) + Font(wp, isBold = TRUE,color="blue") # rowcolumns
#cs2 <- CellStyle(wb) + Font(wb, iscolor="blue")
 
for (i in 1:nfolds)
{
  sheet<-createSheet(wp, sheetName=sheetnames[i])
   Title<-"ISPRS_7 NDVI_AP Classification"
   xlsx.addTitle(wp,sheet, Title)
  
  rows <- createRow(sheet, rowIndex=5);cells <- createCell(rows, colIndex=3)[[1,1]]
  setCellValue(cells,"OA")
   
  # write Overall accuarcy
  addDataFrame(t(AccMatrix$OA[i,]), sheet, startRow=5, startColumn=4,row.names = FALSE, col.names = TRUE,colnamesStyle=cs1,
               rownamesStyle=cs1,byrow=FALSE)
  # write the confusion matrix
  rows <- createRow(sheet, rowIndex=9);cells <- createCell(rows, colIndex=3)[[1,1]]
  setCellValue(cells,"ConfusionMatrix")
  
  #
  addDataFrame(AccMatrix$conf[i], sheet, startRow=10, startColumn=4,row.names = TRUE, col.names = TRUE,colnamesStyle=cs1)
  
  ## write the Producer Accuarcy
  rows <- createRow(sheet, rowIndex=22);cells <- createCell(rows, colIndex=3)[[1,1]]
  setCellValue(cells,"ProducerAccuracy")
  
  #
  addDataFrame(AccMatrix$PA[i], sheet, startRow=23, startColumn=4,row.names = TRUE, col.names = TRUE,colnamesStyle=cs1)
  
  ## write the User Accuarcy
  rows <- createRow(sheet, rowIndex=22);cells <- createCell(rows, colIndex=16)[[1,1]]
  setCellValue(cells,"UserAccuracy")
  
  #
  addDataFrame(AccMatrix$UA[i], sheet, startRow=23, startColumn=17,row.names = TRUE, col.names = TRUE,colnamesStyle=cs1)
  
}
  
saveWorkbook(wp,fname) 
}
# Function for the Title and Font
 xlsx.addTitle<-function(wb, sheet, title){
  rows <-createRow(sheet,rowIndex=3)
   sheetTitle <-createCell(rows, colIndex=8)
  TITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=16, 
                                      color="red", isBold=TRUE, underline=1)
  setCellValue(sheetTitle[[1,1]], title)
   setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)
 }
 