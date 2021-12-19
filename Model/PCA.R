source(paste(getwd(),"/Model/DataPreprocessing.R",sep = "")) # execute DataPreprocessing


trainingSet[-29 ] = scale(trainingSet[-29])
testSet[-29] = scale(testSet[-29])

#applying pca
install.packages('caret')
install.packages('e1071')
library(caret)
library(e1071)

pca = prePrcess(x = trainingSet[-29], method = 'pca', pcaComp=15)
trainingSet = predict(pca, testSet)
trainingSet = trainingSet[c(15:1)]
testSet = predict(pca, testSet)
test
 
