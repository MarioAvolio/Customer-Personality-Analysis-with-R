
source(paste(getwd(),"/Script/DataPreprocessing.R",sep = "")) 

########################################################################
#                                                                      #
#                          CHILDREN ANALISYS                           #
#                                                                      #
########################################################################

ggplot(trainingSet, aes(x=Total_Childs)) + geom_histogram(binwidth = 0.5, colour = "Black")
#NOTE:
# - Most people has only a single child



#--------------------------- Age
ageRange <- cut(trainingSet$Age, breaks = c(24, 64, Inf), include.lowest = T, ordered_result = T, labels = c("Adult", "Senior"))
age_childs_histogram <- ggplot(trainingSet, aes(x=Total_Childs)) + geom_histogram(aes(fill=ageRange), binwidth = 0.5, colour = "Black")
age_childs_histogram
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

incomeRange <- cut(trainingSet$Income, breaks = c(0, 20000, 40000, 60000, 100000), include.lowest = T, ordered_result = T, labels = c("Lower", "Medium", "Hight", "Very Hight"))
incomeRange
income_childs_histogram <- ggplot(trainingSet, aes(x=Total_Childs)) + geom_histogram(aes(fill=incomeRange), binwidth = 0.5, colour = "Black") 
income_childs_histogram
#NOTA:
# - loss income more childs
#--------------------------- 


ggplot(trainingSet, aes(x=Age, y=Total_Childs, colour=Marital_Status, size=Income)) + 
  facet_grid(Marital_Status~Education) + 
  geom_jitter() 
########################################################################





########################################################################
#                                                                      #
#                          CAMPAIGN ANALYSIS                           #
#                                                                      #
########################################################################
ggplot(trainingSet, aes(x=Total_Campains)) + geom_histogram(binwidth = 0.5, colour = "Black")


#Age
ggplot(trainingSet, aes(x=Total_Campains)) + geom_histogram(aes(fill = ))









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








