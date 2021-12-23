########################################################################
#                                                                      #
#                           PRE REQUIREMENT                            #
#                                                                      #
########################################################################
source(paste(getwd(),"/Script/DataPreprocessing.R",sep = "")) 

# Age Range
ageRange <- cut(trainingSet$Age, breaks = c(24, 64, Inf), include.lowest = T, ordered_result = T, labels = c("Adult", "Senior"))
trainingSet <- mutate(trainingSet, Age_range = ageRange)



# Income Range
incomeRange <- cut(trainingSet$Income, 
                  calculateBreaksFromSummary(trainingSet$Income),
                  labels = c("low", "low medium", "medium high", "high"))
trainingSet <- mutate(trainingSet, Income_range = incomeRange)


# SpentRange
spentRange <- cut(trainingSet$Total_spent, 
                  calculateBreaksFromSummary(trainingSet$Total_spent),
                   labels = c("low", "low medium", "medium high", "high"))
trainingSet <- mutate(trainingSet, Spent_range = spentRange)


















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
  geom_jitter() + ylim(0,100000) + geom_smooth()















########################################################################
#                                                                      #
#                          CAMPAIGN ANALYSIS                           #
#                                                                      #
########################################################################
ggplot(trainingSet, aes(x=Total_Campains)) + geom_histogram(binwidth = 0.5, colour = "Black") + xlab("Number of different Campain")


# Create dataAcceptedCmp
dataAcceptedCmp <- data.frame(
  name = c("cmp1", "cmp2", "cmp3", "cmp4", "cmp5") ,  
  value = c(sum(trainingSet$AcceptedCmp1), sum(trainingSet$AcceptedCmp2), sum(trainingSet$AcceptedCmp3),
           sum(trainingSet$AcceptedCmp4), sum(trainingSet$AcceptedCmp5))
)

# Barplot
ggplot(dataAcceptedCmp, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", color = "Black")  



#--------------------------- Age
ggplot(trainingSet, aes(x=Total_Campains)) + geom_histogram(aes(fill=Age_range), binwidth = 0.5, colour = "Black") + facet_grid(Age_range~.) + xlab("Number of different Campain")
#--------------------------- 



#--------------------------- Marital_Status
ggplot(trainingSet, aes(x=Total_Campains)) + geom_histogram(aes(fill=Marital_Status), binwidth = 0.5, colour = "Black") + facet_grid(Marital_Status~.) + xlab("Number of different Campain")
#--------------------------- 



#--------------------------- Total_Childs
ggplot(trainingSet, aes(x=Total_Campains)) + geom_histogram(aes(fill=Total_Childs), binwidth = 0.5, colour = "Black") + facet_grid(Total_Childs~.) + xlab("Number of different Campain")
#--------------------------- 


#--------------------------- Total_Childs
ggplot(trainingSet, aes(x=Total_Campains)) + geom_histogram(aes(fill=Total_Childs), binwidth = 0.5, colour = "Black") + facet_grid(Total_Childs~.) + xlab("Number of different Campain")
#--------------------------- 


#--------------------------- Education
ggplot(trainingSet, aes(x=Total_Campains)) + geom_histogram(aes(fill=Education), binwidth = 0.5, colour = "Black") + facet_grid(Education~.) + xlab("Number of different Campain")
#--------------------------- 


#--------------------------- Income
ggplot(trainingSet, aes(x=Total_Campains)) + geom_histogram(aes(fill=Income_range), binwidth = 0.5, colour = "Black") + facet_grid(Income_range~.) + xlab("Number of different Campain")
#--------------------------- 



########################################################################














########################################################################
#                                                                      #
#               RELATION BETWEEN AGE AND MNT OF THINGS                 #
#                                                                      #
########################################################################





# -------------------------------- TODO: ? QUESTA ROBA ANDREBBE TOLTA - USARE GGPLOT E TRAININGSET



plot(data=customers, aes(x=MntWines)) + geom_density(aes(fill=Marital_Status), position = "stack")

plot(data = customers, customers$Age, customers$MntWines, main="Correlation between Age and Amount spent in Wine", xlab="Age", ylab="Amnt Wine")

plot(density(customers$MntFruits))
plot(customers$Age, customers$MntFruits, main="Correlation between Age and Amount spent in fruit", xlab="Age", ylab="Amnt fruit")

plot(density(customers$MntMeatProducts))
plot(customers$Age, customers$MntMeatProducts, main="Correlation between Age and Amount spent in meat", xlab="Age", ylab="Amnt meat")

plot(density(customers$MntFishProducts))
plot(customers$Age, customers$MntFishProducts, main="Correlation between Age and Amount spent in fish", xlab="Age", ylab="Amnt fish")

plot(density(customers$MntSweetProducts))
plot(customers$Age, customers$MntSweetProducts, main="Correlation between Age and Amount spent in sweet", xlab="Age", ylab="Amnt sweet")



#altre prove di plot, plot lineare (prove)
plot(customers$Age, customers$MntWines, type = "l")                                
lines(customers$Age, customers$MntFruits, type = "l", col = "red")                 
lines(customers$Age, customers$MntMeatProducts, type = "l", col = "green")                 
lines(customers$Age, customers$MntFishProducts, type = "l", col = "blue") 
lines(customers$Age, customers$MntSweetProducts, type = "l", col = "purple") 

cordata = customers[,c('MntWines', 'MntFruits', 'MntMeatProducts','MntFishProducts', 'MntSweetProducts')]

corr <- round(cor(cordata), 1)

corr
#the density plots and normal plots of Mnt are very similar so we can consider an unique variable

customers$Amount_Spent <- customers$MntWines + customers$MntFishProducts + customers$MntFruits +
  customers$MntGoldProds + customers$MntMeatProducts + customers$MntSweetProducts

#RELATION BEETWEEN AGE AND NUM OF THINGS:

plot(density(customers$Age))

plot(customers$Age, customers$NumDealsPurchases, main="Correlation between Age and Num DealsPurchases", xlab="Age", ylab="Num DealsPurchases")
plot(density(customers$NumDealsPurchases))

plot(customers$Age, customers$NumWebPurchases, main="Correlation between Age and Num WebPurchases", xlab="Age", ylab="Num WebPurchases")
plot(density(customers$NumWebPurchases))

plot(customers$Age, customers$NumCatalogPurchases, main="Correlation between Age and Num CatalogPurchases", xlab="Age", ylab="Num CatalogPurchases")
plot(density(customers$NumCatalogPurchases))

plot(customers$Age, customers$NumStorePurchases, main="Correlation between Age and Num StorePurchases", xlab="Age", ylab="Num StorePurchases")
plot(density(customers$NumStorePurchases))

#(prove)
plot(customers$Age, customers$NumDealsPurchases, type = "l")                                 
lines(customers$Age, customers$NumWebPurchases, type = "l", col = "red")                   
lines(customers$Age, customers$NumCatalogPurchases, type = "l", col = "green")            
lines(customers$Age, customers$NumStorePurchases, type = "l", col = "blue") 

legend("topleft",                                       # Add legend to plot
       legend = c("NumDealsPurchases", "NumWebPurchases", "NumCatalogPurchases", "NumStorePurchases"),
       col = c("black", "red", "green", "blue"),
       lty = 1)

#correlation
cordata = customers[,c("NumDealsPurchases", "NumWebPurchases", "NumCatalogPurchases", "NumStorePurchases")]

#APM= A stands for Age, PM for payment method
corrAPM <- round(cor(cordata), 1)

corrAPM




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






