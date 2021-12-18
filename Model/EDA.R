library(dplyr) # glimpse

# Read customers
customers <- read.csv(paste(getwd(),"/Model/marketing_campaign.csv",sep = ""), header=TRUE, sep="\t",  stringsAsFactors=T) # use TAB as separator!
head(customers, n=6)
glimpse(customers)
dim(customers)

#summary statistics
summary(customers) # most important!
class(customers$AcceptedCmp1)


