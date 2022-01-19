# getting pca dataset
source(paste(getwd(),"/Script/PCA.R",sep = "")) 
########################################################################
#                                                                      #
#                               LIBRARY                                #
#                                                                      #
########################################################################
library("rpart")
library("ROCR")

########################################################################



##########################COMPUTE PERFORMANCE###########################


trainingSet_input$Response<-trainingSet$Response
testSet_input$Response<-testSet$Response


classifier <- rpart(Response ~ ., data = trainingSet_input, 
                    method = "class")

y_pred <- predict(classifier, testSet_input, type = "class")

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


#########################10-FOLD CROSS VALIDATION########################


# applying  k-fold cross validation
folds= createFolds(trainingSet_input$Response, k=10)
cv = lapply(folds, function(x){
  
  training_fold= trainingSet_input[-x,]
  test_fold= trainingSet_input[x,]
  # building the classifier
  response.default.tree <- rpart(Response ~ ., data = training_fold,  method = "class")
  
  # predicting values
  response.default.tree.pred <- predict(response.default.tree, newdata = test_fold, type = "class")
 
  cm = table(test_fold[,6], response.default.tree.pred)
  return (cm)
  
})

complex_cf =Reduce('+',  cv)
complex_cf

accuracy = sum(diag(complex_cf))/sum(complex_cf)
precision = (complex_cf[2,2] /sum(complex_cf[2,1], complex_cf[2,2]))
recall = (complex_cf[2,2] /sum(complex_cf[2,2], complex_cf[1,2]))
fmeasure = (2*precision*recall)/(precision+recall)

accuracy
precision
recall
fmeasure

##########################ROC CURVE###########################

dtreefit=  rpart(Response ~ ., data = trainingSet_input, method = "class")
predTree = predict(dtreefit,testSet_input[, !names(testSet_input) %in% c("Response")], probability=TRUE)


predTReeToRoc = predTree[, 2]
predTReeToRoc
predRocr = prediction(predTReeToRoc, testSet_input$Response)
perfRocr = performance(predRocr, measure = "auc", x.measure = "cutoff")
perfTprRocr = performance(predRocr, "tpr","fpr")
plot(perfTprRocr, colorize=T,main=paste("AUC:",(perfRocr@y.values)))
abline(a=0, b=1)

