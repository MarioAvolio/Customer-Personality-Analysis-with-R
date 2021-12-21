########################################################################
#                                                                      #
#                           Read customers                               #
#                                                                      #
######################################################################## 

# source(paste(getwd(),"/Script/DescriptionOfData.R",sep = "")) 

customers <- read.csv(paste(getwd(),"/Data/marketing_campaign.csv",sep = ""), header=TRUE, sep="\t",  stringsAsFactors=F) # use TAB as separator!

########################################################################








########################################################################
#                                                                      #
#                              FACTORIZE                               #
#                                                                      #
########################################################################
customers$Complain<-factor(customers$Complain)
customers$Response<-factor(customers$Response)

#get the column
df <- data.frame(date=customers$Dt_Customer)

#creating a new column in df table extraxcting the year from the df$date column
df$year <- format(as.Date(df$date, format="%d-%m-%Y"),"%Y")

#assign df$year column to Dt_Customer colum
customers$Dt_Customer<-df$year

#factoring the Dt_column
customers$Dt_Customer <- factor(customers$Dt_Customer)

# remove df from memory
remove(df)

########################################################################











########################################################################
#                                                                      #
#                           REFACTOR customers                           #
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
# ------------------------------------- 





# ------------------------------------- TOTAL
#Creating a new variable:Total_spent
customers <- mutate(customers, Total_spent = MntWines + MntFruits + MntMeatProducts + MntFishProducts + MntSweetProducts + MntGoldProds)

# Details about previous campains also combined. Creating a new variable:Total_Campains
customers <- mutate(customers, Total_Campains = AcceptedCmp1 + AcceptedCmp2 + AcceptedCmp3 + AcceptedCmp4 + AcceptedCmp5)

# These variables can be combined and we can get the no of children for the customers. Creating a new variable:Total_Childs
customers <- mutate(customers, Total_Childs = Kidhome + Teenhome)
# ------------------------------------- 



# we can calculate customer age from the birth year. It will be more usefull to our analysis.
# creating a new variable Age from Year of Birth 
thisYear <- as.numeric(format(as.Date(Sys.Date(), format="%d-%m-%Y"),"%Y"))
thisYear
customers <- mutate(customers, Age = thisYear - Year_Birth)


#Dropping some redundant features
# customers <- customers[c(-1,-2,-6,-7,-8,-10,-11,-12,-13,-14,-15,-21,-22,-23,-24,-25,-27,-28)]
customers <- select(customers, - ID, - Year_Birth, - Z_CostContact, - Z_Revenue)
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










# ----------------------------------  Feature Scaling - To implement? Most library implement this
#trainingSet[, c(5,8:15)] <- scale(trainingSet[, c(5,8:15)])
#testSet[, c(5,8:15)] <- scale(testSet[, c(5,8:15)]) # TODO
