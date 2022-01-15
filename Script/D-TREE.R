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
testSet_input$Response<-testSet$Response

# costruisco l'albero
classifier <- rpart(Response ~ ., 
                               data = trainingSet_input, 
                               method = "class")

prp( classifier, 
     type = 1, extra = 1, varlen = -10, 
     box.col = ifelse(classifier$frame$var == "<leaf>", 'gray', 'white'))


y_pred <- predict(classifier, testSet_input, type = "class")

y_pred
cm = table(testSet_input[,6],y_pred)
cm

accuracy<- sum(diag(cm))/sum(cm)

precision<- (cm[2,2] /sum(cm[2,1], cm[2,2]))

recall<- (cm[2,2] /sum(cm[2,2], cm[1,2]))

fmeasure<- (2*precision*recall)/(precision+recall)

accuracy
precision
recall
fmeasure

recall
cv.ct <- rpart( Response ~ ., 
                data = trainingSet_input, 
                method = "class", 
                cp = 0, minsplit = 2, 
                xval = 10)
printcp(cv.ct)

cv.ct


# min error
minerror <- min(cv.ct$cptable[ , 4])
minerror

# corresponding xstd
minerrorstd <- cv.ct$cptable[cv.ct$cptable[,4] == minerror, 5]
minerrorstd

# list of trees where xerror is less than minerror + minerrorstd
simplertrees <- cv.ct$cptable[cv.ct$cptable[,4] < minerror + minerrorstd, ]
simplertrees

# cp of the simplest of those trees
bestcp <- simplertrees[1, 1]
bestcp

response.best.tree <- prune( cv.ct, 
                             cp = bestcp )

prp( response.best.tree, 
     type = 1, extra = 1, varlen = -15, cex = 0.5,
     box.col = ifelse(response.best.tree$frame$var == "<leaf>", 'gray', 'white' ))


response.best.tree.pred <- predict(response.best.tree, testSet_input, type = "class")
confusionMatrix<-confusionMatrix(response.best.tree.pred, as.factor(testSet_input$Response), positive = "1")
confusionMatrix
