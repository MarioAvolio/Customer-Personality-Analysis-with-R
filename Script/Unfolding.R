# execute DataPreprocessing
source(paste(getwd(),"/Script/PCA.R",sep = "")) 
########################################################################
#                                                                      #
#                               LIBRARY                                #
#                                                                      #
########################################################################
library("rpart")
library("caret")
library("rpart.plot")
library("tidyverse")
library("RColorBrewer")
library(xtable)
########################################################################



# nel nuovo dataset preso da pca aggiungo la colonna response
trainingSet_input$Response<-trainingSet$Response

# building the classifier
response.default.tree <- rpart(Response ~ ., data = trainingSet_input,  method = "class")

prp(response.default.tree, 
     type = 1, extra = 1, varlen = -10, 
     box.col = ifelse(response.default.tree$frame$var == "<leaf>", 'gray', 'white'))

# predicting values
response.default.tree.pred <- predict(response.default.tree, trainingSet_input, type = "class")

# making confusion matrix 
confusionMatrix.default<-confusionMatrix(response.default.tree.pred, as.factor(trainingSet_input$Response), positive = "1")
confusionMatrix.default




# applying  l-fold cross validation
folds= createFolds(trainingSet_input$Response, k=10)
cv= lapply(folds, function(x){
  training_fold= trainingSet_input[-x,]
  test_fold= trainingSet_input[x,]
  # building the classifier
  response.default.tree <- rpart(Response ~ ., data = training_fold,  method = "class")
  
  # predicting values
  response.default.tree.pred <- predict(response.default.tree, test_fold, type = "class")
  
  cm= table(test_fold[,3], response.default.tree.pred)
  
  accuracy= (cm[1,1]+cm[2,2]/cm[1,1]+cm[2,2]+cm[2,1]+cm[2,2])
  return(accuracy)
  
})

accuracy= mean(as.numeric(cv))
accuracy

