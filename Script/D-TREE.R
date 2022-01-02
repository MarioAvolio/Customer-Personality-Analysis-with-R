# execute DataPreprocessing
source(paste(getwd(),"/Script/DataPreprocessing.R",sep = "")) 

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


#decision tree considering all variables
decisionTree = rpart(Response ~ ., data=trainingSet, method="class")
rpart.plot(decisionTree)


#decision tree using the first 4 variables
decisionTree = rpart(Response ~ Education + Marital_Status + Income + Total_Childs , data=trainingSet, method="class")

rpart.plot(decisionTree)



#We can now use our decision tree model, already trained, to make a prediction on our test set
trainingSet$Prediction <- predict(decisionTree, trainingSet, type = "class")

confusion.matrix = table(trainingSet$Response, trainingSet$Prediction)

confusion.matrix

sum(diag(confusion.matrix))/sum(confusion.matrix) #equal to 0.861

#summary of the decision tree
summary(decisionTree)

#List complexity parameter
printcp(decisionTree)
 
#plot the decision tree complexity parameter
plotcp(decisionTree)

#NOTE: n short, the greater the CP value, the fewer the number of splits there are.
#     CP is the amount by which splitting that node improved the relative error.

#        CP nsplit rel error  xerror     xstd
#1 0.035581      0   1.00000 1.00000 0.056456
#2 0.010000      2   0.92884 0.94757 0.055208

#So the cp of the root is 0.03+0.01 = 0.04


prunedDecisionTree = prune(decisionTree, cp= 0.05)
rpart.plot(prunedDecisionTree)

#with cp equal to 0.05 there is only one node comparing the original decision tree