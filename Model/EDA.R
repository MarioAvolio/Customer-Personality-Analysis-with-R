library(dplyr) # glimpse
library(ggplot2)
library(naniar)
library(tidyr)
# --------------------------------- UTILITY FUNCTIONS
multiple.func <- function(x) {
  c(min = min(x), mean = mean(x), max = max(x), sd=sd(x), var=var(x),
    median=median(x), range=range(x))
}

# --------------------------------- READ CUSTOMERS DATASET
customers <- read.csv(paste(getwd(),"/Model/marketing_campaign.csv",sep = ""), header=TRUE, sep="\t",  stringsAsFactors=F) # use TAB as separator!

# ---------------------------------- SUMMARY
summary(customers) # most important!
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


# --------------------- TODO
#multi.func su ogni colonna 
sapply(dataSet, multiple.func)


#stampa di tutti gli istogrammi
library(Hmisc)
hist.data.frame(dataSet)


#tentativo di boxplot
x = dataSet[,5:7]
par(mfrow=c(5,7))
for(i in 5:7) {
  boxplot(x[,i], main=names(dataSet)[i]) }

plot(x=dataSet$Year_Birth, y=dataSet$Recency, col=dataSet$AcceptedCmp3)
