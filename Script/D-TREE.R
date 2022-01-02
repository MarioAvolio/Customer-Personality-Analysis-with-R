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
decisionTreeAll = rpart(Response ~ ., data=trainingSet, method="class")
rpart.plot(decisionTreeAll)

#We can now use our decision tree model, already trained, to make a prediction on our test set
trainingSet$Prediction1 <- predict(decisionTreeAll, trainingSet, type = "class")

confusion.matrix1 = table(trainingSet$Response, trainingSet$Prediction1)

confusion.matrix1

#     0    1
# 0 1486   39
# 1  147  120

sum(diag(confusion.matrix1))/sum(confusion.matrix1) 
#equal to 0.8962, it classifies very well

#summary of the decision tree
summary(decisionTreeAll)

#List complexity parameter
printcp(decisionTreeAll)

#        CP    nsplit rel error  xerror     xstd
#1 0.067416      0   1.00000    1.00000   0.056456
#2 0.022472      3   0.77903    0.78652   0.050996
#3 0.014981      5   0.73408    0.79775   0.051310
#4 0.011236      6   0.71910    0.81273   0.051724
#5 0.010000      8   0.69663    0.83521   0.052334

#So the cp of the root is 0.067 + 0.022 + 0.015 + 0.011 + 0.010 = 0,125

#plot the decision tree complexity parameter
plotcp(decisionTreeAll)

#NOTE: n short, the greater the CP value, the fewer the number of splits there are.
#     CP is the amount by which splitting that node improved the relative error.




prunedDecisionTreeAll = prune(decisionTreeAll, cp= 0.05)
rpart.plot(prunedDecisionTreeAll)



####################################################################################

#lets see what are the changes considering the first four variables
#decision tree using the first 4 variables
decisionTreeFew = rpart(Response ~ Education + Marital_Status + Income + Total_Childs , data=trainingSet, method="class")

rpart.plot(decisionTreeFew)



#We can now use our decision tree model, already trained, to make a prediction on our test set
trainingSet$Prediction2 <- predict(decisionTreeFew, trainingSet, type = "class")

confusion.matrix2 = table(trainingSet$Response, trainingSet$Prediction2)

confusion.matrix2

sum(diag(confusion.matrix2))/sum(confusion.matrix2) #equal to 0.861

#summary of the decision tree
summary(decisionTreeFew)

#List complexity parameter
printcp(decisionTreeFew)
 
#plot the decision tree complexity parameter
plotcp(decisionTreeFew)

#NOTE: n short, the greater the CP value, the fewer the number of splits there are.
#     CP is the amount by which splitting that node improved the relative error.

#        CP nsplit rel error  xerror     xstd
#1 0.035581      0   1.00000 1.00000 0.056456
#2 0.010000      2   0.92884 0.94757 0.055208

#So the cp of the root is 0.03+0.01 = 0.04


prunedDecisionTreeFew = prune(decisionTreeFew, cp= 0.05)
rpart.plot(prunedDecisionTreeFew)

#with cp equal to 0.05 there is only one node comparing the original decision tree

#the % of decsionTreeFew is less than decisionTreeAll (3% less).The number of 
# considered variables is lower. 

