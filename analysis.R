# --------------------------------------------------- DATA PROCESSING

# Read DataSet
dataSet <- read.csv("marketing_campaign.csv", header=TRUE, sep="\t") # use TAB as separator!
head(dataSet) # first 6 examples
col(dataSet)
row(dataSet)
View(dataSet)

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


