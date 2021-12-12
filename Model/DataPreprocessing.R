# --------------------------------------------------- DATA PREPROCESSING
# 1 Read DataSet
# 2 Convert categorical data to factor data
# 3 Fix Missing Data with average data
# 4 Split DataSet into Training-Set and Test-Set
# 5 Feature Scaling
# ---------------------------------------------------




# Read DataSet
dataSet <- read.csv("marketing_campaign.csv", header=TRUE, sep="\t",  stringsAsFactors=T) # use TAB as separator!



# Convert categorical to factor
converterCharacterToFactor <- function(column) {
  
  uniqueValue <- levels(column) # take unique value of column
  length(uniqueValue)
  column <- factor(column,
                   levels = uniqueValue,
                   labels = 1:length(uniqueValue)) # Convert character column to factor 
  
}
dataSet$Education <- converterCharacterToFactor(dataSet$Education)
dataSet$Marital_Status <- converterCharacterToFactor(dataSet$Marital_Status)
dataSet$Dt_Customer <- converterCharacterToFactor(dataSet$Dt_Customer)



# Missing Data
# use average for missing data
for(i in 1:ncol(dataSet)) {       # for-loop over columns
  dataSet[ , i] <- ifelse(is.na(dataSet[ ,i]), # is.na check is a value is not available
                          ave(dataSet[, i], FUN = function(x) mean(x, na.rm = TRUE)), # if is not available change with average
                          dataSet[ ,i] # else
                          ) 
  
}



# Split in Training-Set and Test-Set
# install.packages('caTools')
library(caTools)
set.seed(17538)
split <- sample.split(dataSet$ID, SplitRatio = 0.8)
trainingSet <- subset(dataSet, split == TRUE)
testSet <- subset(dataSet, split == FALSE)



# Feature Scaling 
trainingSet[, c(5,8:15)] <- scale(trainingSet[, c(5,8:15)])
testSet[, c(5,8:15)] <- scale(testSet[, c(5,8:15)]) # TODO
