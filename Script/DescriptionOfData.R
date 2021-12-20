library(dplyr) # glimpse
library(ggplot2)
library(naniar)
library(tidyr)
library(lubridate)
library(ggcorrplot)
library(caTools)
library(DataExplorer)


########################################################################
#                                                                      #
#                           Read Dataset                               #
#                                                                      #
######################################################################## 
customers <- read.csv(paste(getwd(),"/Data/marketing_campaign.csv",sep = ""), header=TRUE, sep="\t",  stringsAsFactors=F) # use TAB as separator!
########################################################################






########################################################################
#                                                                      #
#                               SUMMARY                                #
#                                                                      #
########################################################################
summary(customers) # most important!
dim(customers)
head(customers, n=6)
glimpse(customers)
sapply(customers, class)


# Boxplot of the Year of birth variable
ggplot(customers, aes(Year_Birth)) + geom_boxplot()  #aes => Aesthetics layer; geom_boxplot => geometry layer

#NOTE:
# - Dt_Customer that indicates a customer joined the database is not parsed as Date object.
# - We also noted from looking at the summary statistics, the minimum year of birth 1893. This became clear when we plotted a boxplot.
# - There are some categorical features, so we will need to encode into numeric form as we proceed.


# Boxplot of Recency variable
ggplot(customers, aes(Recency)) + geom_boxplot()  #aes => Aesthetics layer; geom_boxplot => geometry layer

#NOTE:
# -

#-----------MNT VARIABLES BOXPLOT

# Boxplot of the Mnt Wines variable
ggplot(customers, aes(MntWines)) + geom_boxplot()  #aes => Aesthetics layer; geom_boxplot => geometry layer

#NOTE:
# - MntWines indicates the amount of wine products.
# - We also noted from looking at the summary statistics, the minimum amount is 0 and max is 1493. The third quantile is 504.25.
# - All theese became clear when we plotted a boxplot. There are also many cases where the mnt of wine is higher compared to the quantiles.


# Boxplot of the Mnt Fruits variable
ggplot(customers, aes(MntFruits)) + geom_boxplot()  #aes => Aesthetics layer; geom_boxplot => geometry layer

#NOTE:
# - MntFruits indicates the amount of fruit products.
# - We also noted from looking at the summary statistics, the minimum amount is 0 and max is 199 The third quantile is 33.0.
# - All theese became clear when we plotted a boxplot. There are also many cases where the mnt of fruit is higher compared to the quantiles.


# Boxplot of the Mnt Meat Products variable
ggplot(customers, aes(MntMeatProducts)) + geom_boxplot()  #aes => Aesthetics layer; geom_boxplot => geometry layer

#NOTE:
# - MntMeatProducts indicates the amount of meat products.
# - We also noted from looking at the summary statistics, the minimum amount is 0 and max is 1725 The third quantile is 232.0.
# - All theese became clear when we plotted a boxplot. There are also many cases where the mnt of meat is higher compared to the quantiles.


# Boxplot of the Mnt Fish Products variable
ggplot(customers, aes(MntFishProducts)) + geom_boxplot()  #aes => Aesthetics layer; geom_boxplot => geometry layer

#NOTE:
# - MntFishProducts indicates the amount of fish products.
# - We also noted from looking at the summary statistics, the minimum amount is 0 and max is 259 The third quantile is 50.0.
# - All theese became clear when we plotted a boxplot. There are also many cases where the mnt of fish  is higher compared to the quantiles.



# Boxplot of the Mnt Sweet Products variable
ggplot(customers, aes(MntSweetProducts)) + geom_boxplot()  #aes => Aesthetics layer; geom_boxplot => geometry layer

#NOTE:
# - MntSweetProducts indicates the amount of sweet products.
# - We also noted from looking at the summary statistics, the minimum amount is 0 and max is 259 The third quantile is 50.0.
# - All theese became clear when we plotted a boxplot. There are also many cases where the mnt of fish  is higher compared to the quantiles.


# Boxplot of the Mnt Gold Prods variable
ggplot(customers, aes(MntGoldProds)) + geom_boxplot()  #aes => Aesthetics layer; geom_boxplot => geometry layer

#NOTE:
# - MntGoldProds indicates the amount of gold products.
# - We also noted from looking at the summary statistics, the minimum amount is 0 and max is 362 The third quantile is 56.0.
# - All theese became clear when we plotted a boxplot. There are also many cases where the mnt of fish  is higher compared to the quantiles.

#- the boxplots of theese variables are similiar.  

########################################################################






########################################################################
#                                                                      #
#                             MISSING VALUE                            #
#                                                                      #
########################################################################
hist(customers$Income,40,col="#adcae6")
ggplot(customers, aes(y = Income)) + geom_boxplot()
n_miss(customers) # counting the total number of missing values in the data
miss_var_summary(customers) # Summarizing missingness in each variable 


# NOTE:
#   
# - There are 24 missing values in the income variable.
# - Also, we can see that the maximum value of the income variable is larger than the 3rd quantile.
########################################################################



########################################################################
#                                                                      #
#                                 MREPORT                              #
#                                                                      #
########################################################################

DataExplorer::create_report(customers, output_dir = paste(getwd(),"/Output/Data/",sep = ""))
########################################################################




















