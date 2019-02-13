library("e1071")
source("ReadXml.R")





### This is the training functin with all region as the training samples
##  and the output is the probability version for each region in testing sample
## the final results are fusion with all prediction in the hierarchical 
##

SVMStandard <- function (testingDataList,trainingDataList,sigma,c) {



  ## For the training data set
  ## Create training data matrix for ksvm
  trainingMatrixList <- list()
  for (i in seq_len(length(trainingDataList))) {
    for (j in seq_len(length(trainingDataList[[i]]))) {
      tmpList <- preorderTraversal(trainingDataList[[i]][[j]])
      ## root of the xml string as the pixel level 
      trainingMatrixList[[length(trainingMatrixList)+1]] <- toList(tmpList[[1]])
    }
  }
  
  trainingDataFrame <- data.frame(matrix(unlist(trainingMatrixList), nrow = length(trainingMatrixList), byrow = T))
  # change to factor
  trainingDataFrame [,1] <- as.factor(trainingDataFrame [,1] )
  # extract the flag root indicator from training data since it's not a feature
  trainingFlagRoot <- trainingDataFrame[2]
  
  trainingDataFrame <- subset(trainingDataFrame, select = -c(X2) )
  ## train a random forest
  model <-   svm(X1~.,data = trainingDataFrame, type="C-classification", kernel =
"radial", gamma=sigma,cost=c, probability = TRUE )



  ## For the testing data set 
  ## Create testing data matrix 
  predictedLabel <- list()
  correctLabel <- list()
  
  for (i in seq_len(length(testingDataList))) {
    for (j in seq_len(length(testingDataList[[i]]))) {
      testingMatrixList <- list()
      tmpList <- preorderTraversal(testingDataList[[i]][[j]])
      testingMatrixList[[1]] <- toList(tmpList[[1]])
      
      
      ## For each testing multilevel tree, predict its subregion with probability 
      ## Then fusion the probability to give a correct answer
      
      testingDataFrame <- data.frame(matrix(unlist(testingMatrixList), nrow = length(testingMatrixList), byrow = T))
      # change to factor
      testingDataFrame [,1] <- as.factor(testingDataFrame [,1] )

      # extract the flag root indicator from training data since it's not a feature
      testingFlagRoot <- testingDataFrame[2]
      

      testingDataFrame <- subset(testingDataFrame, select = -c(X2) )

      ## svm prediction
      predictedProba <- predict(model,subset(testingDataFrame,select = -c(X1)),probability = TRUE)
      
      
      ## find the maximum of predicted probability 
      predictedLabel[length(predictedLabel)+1] <- which.max(attributes(predictedProba)$probabilities)
      ## the element [1,1] containing the correct label of image root region
      ## the value is factor value, should change to numeric value before passing the list 
      correctLabel[length(correctLabel)+1] <- as.numeric(as.character(testingDataFrame[1,1]))
      
    }
  }
  
  
  confusionTable <- table(unlist(predictedLabel),unlist(correctLabel))
  
  return (confusionTable)
  
}









