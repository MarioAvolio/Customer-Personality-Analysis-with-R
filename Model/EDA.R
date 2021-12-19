library(dplyr) # glimpse
library(ggplot2)
library(naniar)

# Read customers
customers <- read.csv(paste(getwd(),"/Model/marketing_campaign.csv",sep = ""), header=TRUE, sep="\t",  stringsAsFactors=T) # use TAB as separator!

# ---------------------------------- SUMMARY
#summary statistics
summary(customers) # most important!
head(customers, n=6)
glimpse(customers)
dim(customers)


# ---------------------------------- MISSING VALUE
# There are 24 missing values in income
ggplot(customers, aes(y = Income)) + geom_boxplot()
n_miss(customers) # counting the total number of missing values in the data
miss_var_summary(customers) # Summarizing missingness in each variable 

# From the above output, we can note the following:
#   
# - Dt_Customer that indicates a cutomer joined the database is not parsed as Date object.
# - There are missing values in the income variable.
# - There are some categorical features, so we will need to encode into numeric form as we proceed.
# - We also noted from looking at the summary statistics, the minimum year of birth 1893. This became clear when we plotted a boxplot.
# - Also, we can see that the maximum value of the income variable is larger than the 3rd quantile.
# - The features Z_CostContact and Z_Revenue show no variation.

