# Function
source(paste(getwd(),"/Script/Functions/Functions.R",sep = "")) 

########################################################################
#                                                                      #
#                           Read dataSet                               #
#                                                                      #
######################################################################## 

source(paste(getwd(),"/Script/DescriptionOfData.R",sep = "")) 

# dataSet <- read.csv(paste(getwd(),"/Data/marketing_campaign.csv",sep = ""), header=TRUE, sep="\t",  stringsAsFactors=F) # use TAB as separator!

########################################################################


########################################################################
#                                                                      #
#                           REFACTOR dataSet                           #
#                                                                      #
########################################################################


# ------------------------------------- COLLAPSING
#Collapsing marital Status into two categories: Single & Couple
unique(dataSet$Marital_Status)
dataSet <- mutate(dataSet, Marital_Status = replace(Marital_Status, Marital_Status == "Divorced" | Marital_Status == "Widow" | Marital_Status == "Alone" | Marital_Status == "Absurd" | Marital_Status == "YOLO", "Single"))
dataSet <- mutate(dataSet, Marital_Status = replace(Marital_Status, Marital_Status == "Together" | Marital_Status == "Married", "Couple"))

#Collapsing the Education into two Categories: graduate and non-graduate
unique(dataSet$Education)
dataSet <- mutate(dataSet, Education = replace(Education, Education == "Graduation"| Education == "PhD" | Education == "Master", "graduate"))
dataSet <- mutate(dataSet, Education = replace(Education, Education == "Basic"| Education == "2n Cycle", "non-graduate"))
# ------------------------------------- 


# ------------------------------------- CONVERSION
#Converting them to factors
dataSet <- mutate(dataSet, Marital_Status = as.factor(Marital_Status), Education = as.factor(Education))

# Encoding the categorical features to numeric
dataSet <- mutate(dataSet, Education = case_when(Education == "graduate" ~ 1,
                                                     Education == "non-graduate" ~ 0))
dataSet <- mutate(dataSet, Marital_Status = case_when(Marital_Status == "Couple" ~ 1,
                                                          Marital_Status == "Single" ~ 0))
# ------------------------------------- 



# ------------------------------------- TOTAL
#Creating a new variable:Total_spent
dataSet <- mutate(dataSet, Total_spent = MntWines + MntFruits + MntMeatProducts + MntFishProducts + MntSweetProducts + MntGoldProds)

# Details about previous campains also combined. Creating a new variable:Total_Campains
dataSet <- mutate(dataSet, Total_Campains = AcceptedCmp1 + AcceptedCmp2 + AcceptedCmp3 + AcceptedCmp4 + AcceptedCmp5)

# These variables can be combined and we can get the no of children for the dataSet. Creating a new variable:Total_Childs
dataSet <- mutate(dataSet, Total_Childs = Kidhome + Teenhome)
# ------------------------------------- 



# we can calculate customer age from the birth year. It will be more usefull to our analysis.
# creating a new variable Age from Year of Birth 
thisYear <- as.numeric(format(as.Date(Sys.Date(), format="%d-%m-%Y"),"%Y"))
thisYear
dataSet <- mutate(dataSet, Age = thisYear - Year_Birth)


#Dropping some redundant features
dataSet <- select(dataSet, - ID, - Year_Birth, - Z_CostContact, - Z_Revenue, -Dt_Customer)
########################################################################


########################################################################
#                                                                      #
#                   SOLVING MISSING DATA INTO INCOME                   #
#                                                                      #
########################################################################
dataSet$Income <- ifelse(is.na(dataSet$Income), # is.na check is a value is not available
                          ave(dataSet$Income, FUN = function(x) mean(x, na.rm = TRUE)), # if is not available change with average
                          dataSet$Income # else
) 
########################################################################


########################################################################
#                                                                      #
#                              SPLITTING                               #
#                                                                      #
########################################################################
set.seed(17538)
split <- sample.split(dataSet$Response, SplitRatio = 0.8)
trainingSet <- subset(dataSet, split == TRUE)
testSet <- subset(dataSet, split == FALSE)
########################################################################


########################################################################
#                                                                      #
#                           FEATURE SCALING                            #
#                                                                      #
########################################################################
trainingSet_scaled <- as.data.frame(scale(trainingSet[, getIndipendentNumbersOfCol()]))
testSet_scaled <- as.data.frame(scale(testSet[, getIndipendentNumbersOfCol()]))
dataSet_scaled <- as.data.frame(scale(dataSet[, getIndipendentNumbersOfCol()]))
########################################################################

