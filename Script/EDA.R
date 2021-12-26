########################################################################
#                                                                      #
#                           PRE REQUIREMENT                            #
#                                                                      #
########################################################################
source(paste(getwd(),"/Script/DataPreprocessing.R",sep = "")) 

# Age Range
ageRange <- cut(trainingSet$Age, breaks = c(24, 64, Inf), include.lowest = T, ordered_result = T, labels = c("Adult", "Senior"))
trainingSet <- mutate(trainingSet, Age_range = ageRange)

ggplot(trainingSet, aes(x="", fill=ageRange))+
  geom_bar(width = 1)+
  coord_polar("y")+theme_void()

#looking the pie chart is clear that the majory are adult. 


# Income Range
incomeRange <- cut(trainingSet$Income, 
                  calculateBreaksFromSummary(trainingSet$Income),
                  labels = c("low", "low medium", "medium high", "high"))
trainingSet <- mutate(trainingSet, Income_range = incomeRange)

ggplot(trainingSet, aes(x="", fill=incomeRange))+
  geom_bar(width = 1)+
  coord_polar("y")+theme_void()

# looking the pie chart the quantity are very similiar.

# SpentRange
spentRange <- cut(trainingSet$Total_spent, 
                  calculateBreaksFromSummary(trainingSet$Total_spent),
                   labels = c("low", "low medium", "medium high", "high"))
trainingSet <- mutate(trainingSet, Spent_range = spentRange)

ggplot(trainingSet, aes(x="", fill=spentRange))+
  geom_bar(width = 1)+
  coord_polar("y")+theme_void()

# looking the pie chart the quantity of people who spend low is equal to high







########################################################################
#                                                                      #
#                          CHILDREN ANALISYS                           #
#                                                                      #
########################################################################

ggplot(trainingSet, aes(x=Total_Childs)) + geom_histogram(binwidth = 0.5, colour = "Black")
#NOTE:
# - Most people has only a single child



#--------------------------- Age
age_childs_histogram <- ggplot(trainingSet, aes(x=Total_Childs)) + geom_histogram(aes(fill=Age_range), binwidth = 0.5, colour = "Black")
age_childs_histogram + facet_grid(Age_range~.)
#--------------------------- 





#--------------------------- Marital_Status
marital_status_childs_histogram <- ggplot(trainingSet, aes(x=Total_Childs)) + geom_histogram(aes(fill=Marital_Status), binwidth = 0.5, colour = "Black")
marital_status_childs_histogram + facet_grid(Marital_Status~.)
#--------------------------- 




#--------------------------- Education

education_childs_histogram <- ggplot(trainingSet, aes(x=Total_Childs)) + geom_histogram(aes(fill=Education), binwidth = 0.5, colour = "Black") 
education_childs_histogram + facet_grid(Education~.)

#NOTA:
# - Graduate people has more children
#--------------------------- 




#--------------------------- Income
income_childs_plot <- ggplot(trainingSet, aes(y=Income, x=Total_Childs)) + geom_jitter() 
income_childs_plot + ylim(0, 100000)

income_childs_histogram <- ggplot(trainingSet, aes(x=Total_Childs)) + geom_histogram(aes(fill=Income_range), binwidth = 0.5, colour = "Black") 
income_childs_histogram + facet_grid(Income_range~.)
#NOTA:
# - loss income more childs
#--------------------------- 


ggplot(trainingSet, aes(x=Age, y=Total_Childs, colour=Marital_Status, size=Income)) + 
  facet_grid(Marital_Status~Education) + 
  geom_jitter() + geom_boxplot(size=0.7, alpha=0.5)
########################################################################






########################################################################
#                                                                      #
#                        TOTAL SPENT ANALYSIS                          #
#                                                                      #
########################################################################

ggplot(trainingSet, aes(x=Total_spent)) + geom_histogram(binwidth = 15, colour = "Black")
#NOTE: most people spend less than 500


# Create dataAcceptedCmp
dataTotalSpent <- data.frame(
  name = c("MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts", "MntSweetProducts", "MntGoldProds"),  
  value = c(sum(trainingSet$MntWines), sum(trainingSet$MntFruits), sum(trainingSet$MntMeatProducts),
            sum(trainingSet$MntFishProducts), sum(trainingSet$MntSweetProducts), sum(trainingSet$MntGoldProds))
)

# Barplot
ggplot(dataTotalSpent, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", color = "Black") + xlab("Total Spent for each type")  



#--------------------------- Age
age_total_spent_histogram <- ggplot(trainingSet, aes(x=Total_spent)) + facet_grid(Age_range~.) 
age_total_spent_histogram + geom_histogram(aes(fill=Age_range), binwidth = 15, color="Black")
age_total_spent_histogram + geom_density(aes(fill=Age_range), position = "Stack")

#NOTE: most people spend less than 500 especially from 30 years to 75 years.
#- individuals under 25, on the other hand, generally spend a total that is greater than 500.
#--------------------------- 





#--------------------------- Marital_Status
marital_status_total_spent_histogram <- ggplot(trainingSet, aes(x=Total_spent)) + facet_grid(Marital_Status~.) 
marital_status_total_spent_histogram + geom_histogram(aes(fill=Marital_Status), binwidth = 15, colour = "Black")
marital_status_total_spent_histogram + geom_density(aes(fill=Marital_Status), position = "Stack")

#NOTE: In this case they look similar. the majority of couples spend a total that is less than 500. 
#- The situation in single is a little more relaxed it will be that most of the individuals 
#- in the dataset are couple
#--------------------------- 







#--------------------------- Education
education_total_spent_histogram <- ggplot(trainingSet, aes(x=Total_spent)) + facet_grid(Education~.)
education_total_spent_histogram + geom_histogram(aes(fill=Education), binwidth = 15, colour = "Black")
education_total_spent_histogram + geom_density(aes(fill=Education), position = "Stack")

#NOTE:
# - Graduate generally spend more than non-graduate. the majority of non-graduate
#- spend from 0 to 1500. from 1500 there are more cases of graduates than non-graduate
#--------------------------- 



#--------------------------- Children
education_total_spent_histogram <- ggplot(trainingSet, aes(x=Total_spent)) + facet_grid(Total_Childs~.)
education_total_spent_histogram + geom_histogram(aes(fill=Total_Childs), binwidth = 15, colour = "Black")
education_total_spent_histogram + geom_density(aes(fill=Total_Childs), position = "Stack")
#--------------------------- 



#--------------------------- Income
income_total_spent_histogram <- ggplot(trainingSet, aes(x=Total_spent)) + geom_histogram(aes(fill=Income_range), binwidth = 15, colour = "Black")
income_total_spent_histogram + facet_grid(Income_range~.)
ggplot(trainingSet, aes(y=Income, x=Total_spent)) + geom_jitter() + geom_smooth()

#--------------------------- 

ggplot(trainingSet, aes(x=Total_spent, y=Income, colour=Marital_Status, size=Income)) + 
  facet_grid(Marital_Status~Education) + 
  geom_jitter() + ylim(0,100000) + geom_smooth() + geom_boxplot(size=0.7, alpha=0.5)


ggplot(trainingSet, aes(x=Total_spent, y=Income, colour=Total_Childs, size=Income)) + 
  facet_grid(Total_Childs~Education) + 
  geom_jitter() + ylim(0,100000) + geom_smooth() + geom_boxplot(size=0.7, alpha=0.5)


ggplot(trainingSet, aes(x=Total_spent, y=Income, colour=Age_range, size=Income)) + 
  facet_grid(Age_range~Education) + 
  geom_jitter() + ylim(0,100000) + geom_smooth() + geom_boxplot(size=0.7, alpha=0.5)

########################################################################










########################################################################
#                                                                      #
#                          CAMPAIGN ANALYSIS                           #
#                                                                      #
########################################################################
ggplot(trainingSet, aes(x=Total_Campains)) + geom_histogram(binwidth = 0.5, colour = "Black") + xlab("Number of different Campain")

#NOTE: the majority didn't accept any campaign.   
# the number of people who accepted 0 campaign is 3 times more than people who accepted 1 campaign.

# Create dataAcceptedCmp
dataAcceptedCmp <- data.frame(
  name = c("cmp1", "cmp2", "cmp3", "cmp4", "cmp5") ,  
  value = c(sum(trainingSet$AcceptedCmp1), sum(trainingSet$AcceptedCmp2), sum(trainingSet$AcceptedCmp3),
           sum(trainingSet$AcceptedCmp4), sum(trainingSet$AcceptedCmp5))
)

# Barplot
ggplot(dataAcceptedCmp, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", color = "Black")  

#NOTE: the campaign 4 is the most accepted. the less accepted is the campaign.
# the 3rd and 5th campaign are equally  accepted.


#--------------------------- Age
ggplot(trainingSet, aes(x=Total_Campains)) + geom_histogram(aes(fill=Age_range), binwidth = 0.5, colour = "Black") + facet_grid(Age_range~.) + xlab("Number of campaign accepted ")

#NOTE: it is evident that the majority of individuals, not distinguishing
# between adults and seniors have accepted 0 campaigns.

#--------------------------- 



#--------------------------- Marital_Status (X)
ggplot(trainingSet, aes(x=Total_Campains)) + geom_histogram(aes(fill=Marital_Status), binwidth = 0.5, colour = "Black") + facet_grid(Marital_Status~.) + xlab("Number of campaign accepted")


#NOTE: 
#--------------------------- 



#--------------------------- Total_Childs
ggplot(trainingSet, aes(x=Total_Campains)) + geom_histogram(aes(fill=Total_Childs), binwidth = 0.5, colour = "Black") + facet_grid(Total_Childs~.) + xlab("Number of campaign accepted")

#NOTE: most instances for which the number of children is 0 have not accepted any campaigns. the values for families with 0 and with 1 child are very similar.
# In the other cases, families with 2 or 3 children, 0 accepted campaigns prevails. 

#--------------------------- 


#--------------------------- Total_spent (X)
ggplot(trainingSet, aes(x=Total_Campains)) + geom_histogram(aes(fill=Total_spent), binwidth = 0.5, colour = "Black") + facet_grid(Total_spent~.) + xlab("Number of campaign accepted")
#--------------------------- 


#--------------------------- Education (X)
ggplot(trainingSet, aes(x=Total_Campains)) + geom_histogram(aes(fill=Education), binwidth = 0.5, colour = "Black") + facet_grid(Education~.) + xlab("Number of campaign accepted")
#--------------------------- 


#--------------------------- Income
ggplot(trainingSet, aes(x=Total_Campains)) + geom_histogram(aes(fill=Income_range), binwidth = 0.5, colour = "Black") + facet_grid(Income_range~.) + xlab("Number of campaign accepted")

#NOTE:  higher the income, the less is the number of instances that accept 0 campaigns, while the number of acceptances for 2, 3 and 4 campaigns increases.
#--------------------------- 



########################################################################







########################################################################
#                                                                      #
#                                 MREPORT                              #
#                                                                      #
########################################################################

# DataExplorer::create_report(customers, output_dir = paste(getwd(),"/Output/Data/",sep = ""), output_file = "reportAfterDataPreprocessing.html")
########################################################################










########################################################################
#                                                                      #
#                           REMOVE TMP DATA                            #
#                                                                      #
########################################################################
trainingSet <- select(trainingSet, -Age_range)
trainingSet <- select(trainingSet, -Income_range)
########################################################################






