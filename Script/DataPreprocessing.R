# Function
source(paste(getwd(),"/Script/Functions/Functions.R",sep = "")) 

########################################################################
#                                                                      #
#                           Read customers                             #
#                                                                      #
######################################################################## 

source(paste(getwd(),"/Script/DescriptionOfData.R",sep = "")) 

# customers <- read.csv(paste(getwd(),"/Data/marketing_campaign.csv",sep = ""), header=TRUE, sep="\t",  stringsAsFactors=F) # use TAB as separator!

########################################################################












########################################################################
#                                                                      #
#                           REFACTOR customers                         #
#                                                                      #
########################################################################


# ------------------------------------- COLLAPSING
#Collapsing marital Status into two categories: Single & Couple
unique(customers$Marital_Status)
customers <- mutate(customers, Marital_Status = replace(Marital_Status, Marital_Status == "Divorced" | Marital_Status == "Widow" | Marital_Status == "Alone" | Marital_Status == "Absurd" | Marital_Status == "YOLO", "Single"))
customers <- mutate(customers, Marital_Status = replace(Marital_Status, Marital_Status == "Together" | Marital_Status == "Married", "Couple"))

#Collapsing the Education into two Categories: graduate and non-graduate
unique(customers$Education)
customers <- mutate(customers, Education = replace(Education, Education == "Graduation"| Education == "PhD" | Education == "Master", "graduate"))
customers <- mutate(customers, Education = replace(Education, Education == "Basic"| Education == "2n Cycle", "non-graduate"))

#Converting them to factors
customers <- mutate(customers, Marital_Status = as.factor(Marital_Status), Education = as.factor(Education))

# Encoding the categorical features to numeric
customers <- mutate(customers, Education = case_when(Education == "graduate" ~ 1,
                                                                  Education == "non-graduate" ~ 0))
customers <- mutate(customers, Marital_Status = case_when(Marital_Status == "Couple" ~ 1,
                                                                       Marital_Status == "Single" ~ 0))
# ------------------------------------- 





# ------------------------------------- TOTAL
#Creating a new variable:Total_spent
customers <- mutate(customers, Total_spent = MntWines + MntFruits + MntMeatProducts + MntFishProducts + MntSweetProducts + MntGoldProds)

# Details about previous campains also combined. Creating a new variable:Total_Campains
customers <- mutate(customers, Total_Campains = AcceptedCmp1 + AcceptedCmp2 + AcceptedCmp3 + AcceptedCmp4 + AcceptedCmp5)
# customers <- mutate(customers, Type_Of_Campains = paste(AcceptedCmp1*1, AcceptedCmp2*2, AcceptedCmp3*3, AcceptedCmp4*4, AcceptedCmp5*5, sep=", "))


# These variables can be combined and we can get the no of children for the customers. Creating a new variable:Total_Childs
customers <- mutate(customers, Total_Childs = Kidhome + Teenhome)
# ------------------------------------- 



# we can calculate customer age from the birth year. It will be more usefull to our analysis.
# creating a new variable Age from Year of Birth 
thisYear <- as.numeric(format(as.Date(Sys.Date(), format="%d-%m-%Y"),"%Y"))
thisYear
customers <- mutate(customers, Age = thisYear - Year_Birth)


#Dropping some redundant features
customers <- select(customers, - ID, - Year_Birth, - Z_CostContact, - Z_Revenue, -Dt_Customer)
########################################################################









########################################################################
#                                                                      #
#                   SOLVING MISSING DATA INTO INCOME                   #
#                                                                      #
########################################################################
customers$Income <- ifelse(is.na(customers$Income), # is.na check is a value is not available
                          ave(customers$Income, FUN = function(x) mean(x, na.rm = TRUE)), # if is not available change with average
                          customers$Income # else
) 
########################################################################











########################################################################
#                                                                      #
#                              SPLITTING                               #
#                                                                      #
########################################################################
set.seed(17538)
split <- sample.split(customers$Response, SplitRatio = 0.8)
trainingSet <- subset(customers, split == TRUE)
testSet <- subset(customers, split == FALSE)
########################################################################






########################################################################
#                                                                      #
#                           FEATURE SCALING                            #
#                                                                      #
########################################################################

trainingSet_copy_pre <- preProcess(trainingSet[,getIndipendentNumbersOfCol()], method = c("center", "scale"), thresh = 0.70)
trainingSet_copy <- predict(trainingSet_copy_pre, trainingSet[,getIndipendentNumbersOfCol()])
summary(trainingSet_copy)

?preProcess

# trainingSet[, getIndipendentNumbersOfCol()] <- scale(trainingSet[, getIndipendentNumbersOfCol()])
# testSet[, getIndipendentNumbersOfCol()] <- scale(testSet[, getIndipendentNumbersOfCol()])
########################################################################

