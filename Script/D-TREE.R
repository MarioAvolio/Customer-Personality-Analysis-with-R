# execute DataPreprocessing
source(paste(getwd(),"/Script/PCA.R",sep = "")) 
########################################################################
#                                                                      #
#                               LIBRARY                                #
#                                                                      #
########################################################################
library("rpart")
library("rpart.plot")
library("tidyverse")
library("RColorBrewer")
########################################################################



# nel nuovo dataset preso da pca aggiungo la colonna response
trainingSet_input$Response<-trainingSet$Response

# costruisco l'albero
response.default.tree <- rpart(Response ~ ., 
                               data = trainingSet_input, 
                           method = "class")
?rpart



?prp

prp( response.default.tree, 
     type = 1, extra = 1, varlen = -10, 
     box.col = ifelse(response.default.tree$frame$var == "<leaf>", 'gray', 'white'))


response.default.tree.pred <- predict(response.default.tree, trainingSet_input, type = "class")
confusionMatrix.default<-confusionMatrix(response.default.tree.pred, as.factor(trainingSet_input$Response), positive = "1")
confusionMatrix.default




cv.ct <- rpart( Response ~ ., 
                data = trainingSet_input, 
                method = "class", 
                cp = 0, minsplit = 2, 
                xval = 10)
printcp(cv.ct)


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


response.best.tree.pred <- predict(response.best.tree, trainingSet_input, type = "class")
confusionMatrix2<-confusionMatrix(response.best.tree, as.factor(trainingSet_input$Response), positive = "1")

