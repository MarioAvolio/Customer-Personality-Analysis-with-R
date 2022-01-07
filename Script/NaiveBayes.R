source(paste(getwd(),"/Script/DataPreprocessing.R",sep = "")) # execute DataPreprocessing

trainingSet[-29]=scale(trainingSet[-29])
testSet[-29]=scale(testSet[-29])

library(e1071)
classifier= naiveBayes(x=trainingSet[-29], y=trainingSet$Response)

#Predicting the test results
y_pred=predict(classifier, newdata=testSet[-29])

#Making the confusion matrix

cm= table(testSet[,29], y_pred)


#Visualising the training set result

