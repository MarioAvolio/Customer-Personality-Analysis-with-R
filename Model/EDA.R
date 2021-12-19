library(dplyr) # glimpse
library(ggplot2)
library(naniar)
library(tidyr)
library(lubridate)
library(ggcorrplot)
# --------------------------------- UTILITY FUNCTIONS
multiple.func <- function(x) {
  c(min = min(x), mean = mean(x), max = max(x), sd=sd(x), var=var(x),
    median=median(x), range=range(x))
}

# --------------------------------- READ CUSTOMERS DATASET
customers <- read.csv(paste(getwd(),"/Model/marketing_campaign.csv",sep = ""), header=TRUE, sep="\t",  stringsAsFactors=F) # use TAB as separator!

#------------------------------ RELATION BEETWEEN AGE AND MNT OF THINGS:

customers['Age']= 2021-customers$Year_Birth

plot(density(customers$MntWines))

plot(customers$Age, customers$MntWines, main="Correlation between Age and Amount spent in Wine", xlab="Age", ylab="Amnt Wine")

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

#--------------------RELATION BEETWEEN AGE AND NUM OF THINGS:

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


#-----------------------------FACTORIZE

customers$Year_Birth<-factor(customers$Year_Birth)
customers$Education<-factor(customers$Education)
customers$Marital_Status<-factor(customers$Marital_Status)
customers$Kidhome<-factor(customers$Kidhome)
customers$Teenhome<-factor(customers$Teenhome)


#get the column
df <- data.frame(date=customers$Dt_Customer)


#creating a new column in df table extraxcting the year from the df$date column
df$year <- format(as.Date(df$date, format="%d-%m-%Y"),"%Y")

#assign df$year column to Dt_Customer colum
customers$Dt_Customer<-df$year

#factoring the Dt_column
customers$Dt_Customer<-factor(customers$Dt_Customer)

summary(customers[2:29]) # most important!



customers$AcceptedCmp1<-factor(customers$AcceptedCmp1)
customers$AcceptedCmp2<-factor(customers$AcceptedCmp2)
customers$AcceptedCmp3<-factor(customers$AcceptedCmp3)
customers$AcceptedCmp4<-factor(customers$AcceptedCmp4)
customers$AcceptedCmp5<-factor(customers$AcceptedCmp5)
customers$Complain<-factor(customers$Complain)
customers$Response<-factor(customers$Response)

#-----------------------------------------
# ---------------------------------- SUMMARY
summary(customers[2:29]) # most important!
dim(customers)
head(customers, n=6)
glimpse(customers)
sapply(dataSet, class)
# NOTE:
# - There are some categorical features, so we will need to encode into numeric form as we proceed.

# --------------------------------- ANALISYS OF EACH VARIABLE
# Z_CostContact and Z_Revenue
ggplot(customers, aes(Z_CostContact)) + geom_boxplot() 
multiple.func(customers$Z_CostContact)
ggplot(customers, aes(Z_Revenue)) + geom_boxplot() 
multiple.func(customers$Z_Revenue)

# NOTE:
# - The features Z_CostContact and Z_Revenue show no variation.


# Boxplot of the Year of birth variable
ggplot(customers, aes(Year_Birth)) + geom_boxplot()  #aes => Aesthetics layer; geom_boxplot => geometry layer

#NOTE:
# - Dt_Customer that indicates a cutomer joined the database is not parsed as Date object.
# - We also noted from looking at the summary statistics, the minimum year of birth 1893. This became clear when we plotted a boxplot.



# ---------------------------------- MISSING VALUE
hist(customers$Income,40,col="#adcae6")
ggplot(customers, aes(y = Income)) + geom_boxplot()
n_miss(customers) # counting the total number of missing values in the data
miss_var_summary(customers) # Summarizing missingness in each variable 


# NOTE:
#   
# - There are 24 missing values in the income variable.
# - Also, we can see that the maximum value of the income variable is larger than the 3rd quantile.


