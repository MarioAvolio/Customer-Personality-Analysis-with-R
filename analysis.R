# --------------------------------------------------- DATA PROCESSING

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
  
  print(paste(c("coloumn n.", i), collapse = " "))
  dataSet[ , i] <- ifelse(is.na(dataSet[ ,i]), # is.na check is a value is not available
                          ave(dataSet[, i], FUN = function(x) mean(x, na.rm = TRUE)), # if is not available change with average
                          dataSet[ ,i] # else
                          ) 
  
}




# split in Training-Set and Test-Set
install.packages('caTools')
library(caTools)













# --------------------------------------------------- PRINCIPAL COMPONENT ANALISYS - PCA


