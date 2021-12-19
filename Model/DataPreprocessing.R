library(caTools)

# --------------------------------- READ CUSTOMERS DATASET
customers <- read.csv(paste(getwd(),"/Model/marketing_campaign.csv",sep = ""), header=TRUE, sep="\t",  stringsAsFactors=F) # use TAB as separator!



# ----------------------------------  Convert categorical to factor
converterCharacterToFactor <- function(column) {
  
  uniqueValue <- levels(column) # take unique value of column
  length(uniqueValue)
  column <- factor(column,
                   levels = uniqueValue,
                   labels = 1:length(uniqueValue)) # Convert character column to factor 
  
  return(column)
  
}

for(i in 1:ncol(customers)) {       # for-loop over columns
  
  # print(paste(i,class(customers[, i])))
  
  if (is.factor(customers[, i])){
    print(i)
    customers[, i] <- converterCharacterToFactor(customers[, i])
  }
}

# Alternative
# customers$Education <- converterCharacterToFactor(customers$Education)
# customers$Marital_Status <- converterCharacterToFactor(customers$Marital_Status)
# customers$Dt_Customer <- converterCharacterToFactor(customers$Dt_Customer)


# ---------------------------------- REFACTOR DATASET

#categorize education 
customers$Education[customers$Education == "2n Cycle"] = "UG"
customers$Education[customers$Education == "Basic"] = "UG"
customers$Education[customers$Education == "Graduation"] = "PG"
customers$Education[customers$Education == "Master"] = "PG"
customers$Education[customers$Education == "PhD"] = "PG"

#categorize marital status 
customers$Marital_Status[customers$Marital_Status == "Divorced"] = "Single"
customers$Marital_Status[customers$Marital_Status == "Absurd"] = "Single"
customers$Marital_Status[customers$Marital_Status == "YOLO"] = "Single"
customers$Marital_Status[customers$Marital_Status == "Widow"] = "Single"
customers$Marital_Status[customers$Marital_Status == "Together"] = "Couple"
customers$Marital_Status[customers$Marital_Status == "Married"] = "Couple"
customers$Marital_Status[customers$Marital_Status == "Alone"] = "Single"

#total spent 
customers$Amount_Spent <- customers$MntWines + customers$MntFishProducts + customers$MntFruits +
  customers$MntGoldProds + customers$MntMeatProducts + customers$MntSweetProducts

#total purchaes
data$Num_Purchases_made <- data$NumWebPurchases + data$NumCatalogPurchases +
  data$NumStorePurchases

# we can calculate customer age from the birth year. It will be more usefull to our analysis.
customers['Age']= 2021-customers$Year_Birth

# These variables can be combined and we can get the no of children for the customers.
customers['Child']=customers$Kidhome+customers$Teenhome



# ---------------------------------- SOLVING MISSING DATA INTO INCOME
customers$Income <- ifelse(is.na(customers$Income), # is.na check is a value is not available
                          ave(customers$Income, FUN = function(x) mean(x, na.rm = TRUE)), # if is not available change with average
                          customers$Income # else
) 


# ---------------------------------- Split in Training-Set and Test-Set
# install.packages('caTools')
set.seed(17538)
split <- sample.split(dataSet$Response, SplitRatio = 0.8)
trainingSet <- subset(dataSet, split == TRUE)
testSet <- subset(dataSet, split == FALSE)



# ----------------------------------  Feature Scaling - To implement? Most library implement this
#trainingSet[, c(5,8:15)] <- scale(trainingSet[, c(5,8:15)])
#testSet[, c(5,8:15)] <- scale(testSet[, c(5,8:15)]) # TODO
