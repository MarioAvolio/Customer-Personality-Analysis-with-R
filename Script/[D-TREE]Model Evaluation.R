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



##########################COMPUTE PERFORMANCE###########################


trainingSet_input$Response<-trainingSet$Response
testSet_input$Response<-testSet$Response

# costruisco il classificatore 
classifier <- rpart(Response ~ ., data = trainingSet_input, 
                    method = "class")

y_pred <- predict(classifier, testSet_input, type = "class")

y_pred

cm<- confusionMatrix(y_pred, as.factor(testSet_input$Response), positive = "1", mode = "prec_recall")
cm

# Reference
#Prediction   0   1
#          0 366  59
#          1  15   8

#Accuracy : 0.8348         
#Precision : 0.34783        
#Recall : 0.11940        
#F1 : 0.17778        
#Prevalence : 0.14955                    
#Detection Prevalence : 0.05134        
#Balanced Accuracy : 0.54002        
#'Positive' Class : 1      



# applying  l-fold cross validation
folds= createFolds(trainingSet_input$Response, k=10)

cv= lapply(folds, function(x){
  training_fold= trainingSet_input[-x,]
  test_fold= trainingSet_input[x,]
  # building the classifier
  response.default.tree <- rpart(Response ~ ., data = training_fold,  method = "class")
  
  # predicting values
  response.default.tree.pred <- predict(response.default.tree, newdata =  test_fold, type = "class")
 
  cm= table(test_fold[,6], response.default.tree.pred)
  

  return (cm)
  
})

results=as.array(cv)

#  complex_cf= 
#  accuracy=sum(diag(complex_cf))/sum(complex_cf)
#  precision= (complex_cf[2,2] /sum(complex_cf[2,1], complex_cf[2,2]))
#  recall= (complex_cf[2,2] /sum(complex_cf[2,2], complex_cf[1,2]))
#  fmeasure= (2*precision*recall)/(precision+recall)

install.packages("ROCR")
library(ROCR)
dtreefit=rpart(Response ~ ., data = trainingSet_input, 
             method = "class")
pred=predict(dtreefit, probability=TRUE)
pred.prob = attr(pred, "probabilities")
pred.to.roc = pred.prob[, 2]
pred.rocr = prediction(pred.to.roc, testSet_input$Response)
perf.rocr = performance(pred.rocr, measure = "auc", x.measure = "cutoff")
perf.tpr.rocr = performance(pred.rocr, "tpr","fpr")