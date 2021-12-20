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

DataExplorer::create_report(customers, output_dir = paste(getwd(),"/Output/Data/",sep = ""), output_format = "pdf_document")
########################################################################




















